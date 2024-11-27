import pandas as pd
import numpy as np
from scipy.optimize import minimize
import matplotlib.pyplot as plt
import seaborn as sns

# Submitted by Tarun Kumar Sahu SR No. 23156

def load_and_preprocess_data(file_path):
    """
    Load and preprocess the cricket match data from a CSV file.
    
    Args:
    file_path (str): Path to the CSV file containing match data.
    
    Returns:
    pandas.DataFrame: Preprocessed data containing only first innings information.
    """
    # Load the CSV file
    df = pd.read_csv(file_path)
    
    # Calculate remaining runs and overs
    df['remaining_runs'] = df['Innings.Total.Runs'] - df['Total.Runs']
    df['remaining_overs'] = 50 - df['Over']
    
    # Filter for first innings data only
    first_innings = df[df['Innings'] == 1]
    
    # Return relevant columns
    return first_innings[['remaining_overs', 'Wickets.in.Hand', 'remaining_runs']]

def run_production_function(Z0, L, u):
    """
    Calculate the expected runs based on the Duckworth-Lewis-Stern method.
    
    Args:
    Z0 (float): Asymptotic value of the run production function.
    L (float): Rate parameter.
    u (float or array): Number of overs remaining.
    
    Returns:
    float or array: Expected runs to be scored.
    """
    # Prevent division by zero
    Z0 = np.maximum(Z0, 1e-10)
    
    # DLS method formula
    return Z0 * (1 - np.exp(-L * u / Z0))

def safe_log(x):
    """
    Compute logarithm safely by avoiding log(0).
    
    Args:
    x (float or array): Input value(s).
    
    Returns:
    float or array: Safe logarithm of input.
    """
    return np.log(np.maximum(x, 1e-10))

def loss_function(y_true, y_pred):
    """
    Calculate the Poisson log-likelihood loss between true and predicted values.
    
    Args:
    y_true (array): True values.
    y_pred (array): Predicted values.
    
    Returns:
    float: Computed loss.
    """
    y_true = np.maximum(y_true, 0)
    y_pred = np.maximum(y_pred, 0)
    return np.sum((y_pred + 1) * safe_log((y_pred + 1) / (y_true + 1)) - y_pred + y_true)

def objective_function(params, u, w, y):
    """
    Objective function to be minimized in the optimization process.
    
    Args:
    params (array): Model parameters [Z0, L].
    u (array): Overs remaining.
    w (array): Wickets in hand.
    y (array): True runs scored.
    
    Returns:
    float: Loss value.
    """
    Z0, L = params
    y_pred = run_production_function(Z0, L, u)
    return loss_function(y, y_pred)

def fit_preliminary_model(data):
    """
    Fit the preliminary model to estimate Z0 and L for each wicket.
    
    Args:
    data (pandas.DataFrame): Preprocessed cricket match data.
    
    Returns:
    numpy.array: Array of [Z0, L] pairs for each wicket.
    """
    results = []
    for w in range(1, 11):
        wicket_data = data[data['Wickets.in.Hand'] == w]
        if len(wicket_data) == 0:
            results.append([1e-10, 1e-10])  # Default values if no data
            continue
        
        initial_params = [100, 1]  # Initial guess for Z0 and L
        result = minimize(
            objective_function,
            initial_params,
            args=(wicket_data['remaining_overs'].values, wicket_data['Wickets.in.Hand'].values, wicket_data['remaining_runs'].values),
            method='L-BFGS-B',
            bounds=[(1e-10, None), (1e-10, None)]  # Prevent zero values
        )
        results.append(result.x)
    return np.array(results)

def calculate_common_L(preliminary_params, data):
    """
    Calculate a common L value as a weighted average of preliminary L values.
    
    Args:
    preliminary_params (numpy.array): Preliminary model parameters.
    data (pandas.DataFrame): Preprocessed cricket match data.
    
    Returns:
    float: Common L value.
    """
    L_values = preliminary_params[:, 1]
    weights = [len(data[data['Wickets.in.Hand'] == w]) for w in range(1, 11)]
    return np.average(L_values, weights=weights)

def fit_final_model(data, common_L):
    """
    Fit the final model to estimate Z0 for each wicket using the common L.
    
    Args:
    data (pandas.DataFrame): Preprocessed cricket match data.
    common_L (float): Common L value calculated from preliminary model.
    
    Returns:
    numpy.array: Array of Z0 values for each wicket.
    """
    results = []
    for w in range(1, 11):
        wicket_data = data[data['Wickets.in.Hand'] == w]
        if len(wicket_data) == 0:
            results.append(1e-10)  # Default value if no data
            continue
        
        initial_Z0 = 100  # Initial guess for Z0
        result = minimize(
            lambda params: objective_function([params[0], common_L], wicket_data['remaining_overs'].values, wicket_data['Wickets.in.Hand'].values, wicket_data['remaining_runs'].values),
            [initial_Z0],
            method='L-BFGS-B',
            bounds=[(1e-10, None)]  # Prevent zero values
        )
        results.append(result.x[0])
    return np.array(results)


def plot_run_production_functions(params, common_L=None, title="Run Production Functions"):
    """
    Plot the run production functions for different wickets in hand and save the plot.
    
    Args:
    params (numpy.array): Model parameters (Z0 and L for preliminary, Z0 for final).
    common_L (float, optional): Common L value for the final model.
    title (str): Title of the plot.
    """
    plt.figure(figsize=(12, 8))
    colors = sns.color_palette("tab10", 10)
    overs = np.linspace(0, 50, 104)  # Extend range for buffer
    
    max_score = 0
    for w in range(1, 11):
        if common_L is None:
            # Preliminary model: params is a 2D array with Z0 and L for each wicket
            Z0, L = params[w-1]
        else:
            # Final model: params is a 1D array of Z0 values, common_L is used for all wickets
            Z0 = params[w-1]
            L = common_L
        
        scores = run_production_function(Z0, L, np.maximum(50 - overs, 0))  # Ensure non-negative values
        plt.plot(overs, scores, label=f'{w} wickets', color=colors[w-1])
        max_score = max(max_score, np.max(scores))
    
    plt.xlabel('Overs completed', fontsize=14)
    plt.ylabel('Expected runs to be scored', fontsize=14)
    plt.title(title, fontsize=16)
    plt.legend(title='Wickets in hand', bbox_to_anchor=(1.05, 1), loc='upper left', fontsize=12)
    plt.grid(True, linestyle='--', alpha=0.7)
    plt.xlim(-2, 52)  # X-axis from -2 to 52 overs for buffer
    plt.xticks(range(0, 51, 5))  # Keep ticks from 0 to 50
    
    y_max = (int(max_score) // 25 + 1) * 25
    plt.ylim(0, y_max)
    plt.yticks(range(0, y_max + 1, 25))
    
    plt.tight_layout()
    
    # Save the plot
    filename = f'{title.lower().replace(" ", "_")}.png'
    plt.savefig(filename, dpi=300, bbox_inches='tight')
    print(f"Plot saved as {filename}")
    
    plt.show()



def calculate_normalized_loss(params, data, common_L=None):
    """
    Calculate the normalized loss for the model.
    
    Args:
    params (numpy.array): Model parameters.
    data (pandas.DataFrame): Preprocessed cricket match data.
    common_L (float, optional): Common L value for the final model.
    
    Returns:
    float: Normalized loss value.
    """
    total_loss = 0
    total_points = len(data)
    
    for w in range(1, 11):
        wicket_data = data[data['Wickets.in.Hand'] == w]
        if common_L is None:
            Z0, L = params[w-1]
        else:
            Z0, L = params[w-1], common_L
        y_pred = run_production_function(Z0, L, wicket_data['remaining_overs'].values)
        total_loss += loss_function(wicket_data['remaining_runs'].values, y_pred)
    
    return total_loss / total_points


# Main execution
if __name__ == "__main__":
    # Load and preprocess the data
    data = load_and_preprocess_data('/data/tarunks/Data Analytics/Assignment-4/04_cricket_1999to2011.csv')

    # Fit preliminary model
    preliminary_params = fit_preliminary_model(data)
    
    # Plot and save preliminary run-production functions
    plot_run_production_functions(preliminary_params, title="Preliminary Run Production Functions")
    
    # Report the 20 parameters associated with preliminary model
    print("Preliminary parameters (20 in total: Z0 and L for each wicket):")
    for w, params in enumerate(preliminary_params, 1):
        print(f"Wicket {w}: Z0 = {params[0]:.2f}, L = {params[1]:.2f}")

    # Calculate common L and fit final model
    common_L = calculate_common_L(preliminary_params, data)
    final_Z0_params = fit_final_model(data, common_L)
    
    # Plot and save final run-production functions
    plot_run_production_functions(final_Z0_params, common_L, title="Final Run Production Functions")
    
    # Report the 11 parameters associated with final model
    print("\nFinal parameters (11 in total: 10 Z0 values and 1 common L):")
    for w, Z0 in enumerate(final_Z0_params, 1):
        print(f"Wicket {w}: Z0 = {Z0:.2f}")
    print(f"Common L = {common_L:.2f}")

    # Calculate and report normalized loss
    preliminary_loss = calculate_normalized_loss(preliminary_params, data)
    final_loss = calculate_normalized_loss(final_Z0_params, data, common_L)
    print(f"\nPreliminary normalized loss: {preliminary_loss:.4f}")
    print(f"Final normalized loss: {final_loss:.4f}")

    