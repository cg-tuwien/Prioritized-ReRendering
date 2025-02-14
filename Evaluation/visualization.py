import numpy as np
import matplotlib.pyplot as plt
import matplotlib.cm as cm
from matplotlib import colormaps
import os
import glob
import re
from PIL import Image


def import_and_plot_scalar_field(number, scene):
    dirFilename = "prioDir" + str(number) + "_" + str(scene) + ".bin"
    indFilename = "prioInd" + str(number) + "_" + str(scene) + ".bin"
    with open(dirFilename, 'rb') as f:
        # Read width and height
        width = np.fromfile(f, dtype=np.uint32, count=1)[0]
        height = np.fromfile(f, dtype=np.uint32, count=1)[0]

        # Read the scalar field data
        prioDir = np.fromfile(f, dtype=np.uint32).reshape((height, width))
    with open(indFilename, 'rb') as f:
        # Read width and height
        width = np.fromfile(f, dtype=np.uint32, count=1)[0]
        height = np.fromfile(f, dtype=np.uint32, count=1)[0]

        # Read the scalar field data
        prioInd = np.fromfile(f, dtype=np.uint32).reshape((height, width))

    prioComb = 5*prioDir + prioInd

    # Create a figure with two subplots side by side (1 row, 2 columns)
    fig, axes = plt.subplots(1, 3, figsize=(10, 5))

    # Display the first image in the first subplot
    im0 = axes[0].imshow(prioDir, cmap='viridis')
    axes[0].set_title('Direct Priority')
    cbar0 = fig.colorbar(im0, ax=axes[0], orientation='vertical')
    cbar0.set_label('Value')

    # Display the second image in the second subplot
    im1 = axes[1].imshow(prioInd, cmap='viridis')
    axes[1].set_title('Indirect Priority')
    cbar1 = fig.colorbar(im1, ax=axes[1], orientation='vertical')
    cbar1.set_label('Value')

    im2 = axes[2].imshow(prioComb, cmap='viridis')
    axes[2].set_title('Combined Priority')
    cbar2 = fig.colorbar(im2, ax=axes[2], orientation='vertical')
    cbar2.set_label('Value')

    # Adjust layout
    plt.tight_layout()

    # Show the figure
    plt.show()

    # Create the plot
    fig, ax = plt.subplots(figsize=(120, 68), dpi=10)
    cax = ax.imshow(prioComb, cmap='viridis')

    # Remove axes, ticks, and labels
    ax.axis('off')

    # Remove margins
    plt.subplots_adjust(left=0, right=1, top=1, bottom=0)

    # Save the figure
    plt.savefig('prio_' + str(scene) + '.png', bbox_inches='tight', pad_inches=0, )
    plt.close()


def spiral(target_x, target_y):
    chebyshev_distances = np.fromfunction(
        lambda x, y: np.maximum(np.abs(x - target_x), np.abs(y - target_y)),
        (68, 120),
        dtype=int
    )

    # Find the maximum distance
    max_distance = np.max(chebyshev_distances)

    # Invert the Chebyshev distance array
    inverted_chebyshev_distances = max_distance - chebyshev_distances

    # Create the plot
    fig, ax = plt.subplots(figsize=(120, 68), dpi=10)
    cax = ax.imshow(inverted_chebyshev_distances, cmap='viridis')

    # Remove axes, ticks, and labels
    ax.axis('off')

    # Remove margins
    plt.subplots_adjust(left=0, right=1, top=1, bottom=0)

    # Save the figure
    plt.savefig('spiral_' + str(target_x) + '.png', bbox_inches='tight', pad_inches=0, )
    plt.show()


def double_spiral(target1_x, target1_y, target2_x, target2_y):
    chebyshev_distances1 = np.fromfunction(
        lambda x, y: np.maximum(np.abs(x - target1_x), np.abs(y - target1_y)),
        (68, 120),
        dtype=int
    )

    chebyshev_distances2 = np.fromfunction(
        lambda x, y: np.maximum(np.abs(x - target2_x), np.abs(y - target2_y)),
        (68, 120),
        dtype=int
    )

    spiralfield = np.minimum(chebyshev_distances1, chebyshev_distances2)

    # Find the maximum distance
    max_distance = np.max(spiralfield)

    # Invert the Chebyshev distance array
    inverted_chebyshev_distances = max_distance - spiralfield

    # Create the plot
    fig, ax = plt.subplots(figsize=(120, 68), dpi=10)
    cax = ax.imshow(inverted_chebyshev_distances, cmap='viridis')

    # Remove axes, ticks, and labels
    ax.axis('off')

    # Remove margins
    plt.subplots_adjust(left=0, right=1, top=1, bottom=0)

    # Save the figure
    plt.savefig('eyetracking_' + str(target1_x) + '.png', bbox_inches='tight', pad_inches=0, )
    plt.show()


def globalprio():
    scalar_field = np.zeros((68, 120))

    # Create the plot
    fig, ax = plt.subplots(figsize=(120, 68), dpi=10)
    cax = ax.imshow(scalar_field, cmap='viridis')

    # Remove axes, ticks, and labels
    ax.axis('off')

    # Remove margins
    plt.subplots_adjust(left=0, right=1, top=1, bottom=0)

    # Save the figure
    plt.savefig('global.png', bbox_inches='tight', pad_inches=0, )
    plt.show()


def extract_number_from_filename(filename):
    # Define the regular expression pattern
    pattern = r'prioDir(\d+)_([a-zA-Z]*)\.bin'

    # Search for the pattern in the filename
    match = re.search(pattern, filename)

    # If a match is found, return the number as an integer
    if match:
        number = int(match.group(1))
        arbitrary_string = match.group(2)
        return number, arbitrary_string
    else:
        return None, None


def plot_scale():
    # Generate the Viridis colormap
    viridis = cm.get_cmap('viridis', 256)

    # Create a figure and axis for the color scale
    fig, ax = plt.subplots(figsize=(6, 0.5))
    fig.subplots_adjust(bottom=0.5)

    # Create the colorbar
    cbar = fig.colorbar(cm.ScalarMappable(cmap=viridis), cax=ax, orientation='horizontal')

    # Remove default tick labels
    cbar.set_ticks([0,1])
    cbar.set_ticklabels(['', ''])

    # Manually add text labels at desired positions
    ax.text(0.06, -0.85, '0 (Low)', transform=ax.transAxes, ha='center', va='center', fontsize=12)
    ax.text(0.94, -0.85, '(High) 1', transform=ax.transAxes, ha='center', va='center', fontsize=12)
    ax.text(0.5, -0.7, 'RMSE', transform=ax.transAxes, ha='center', va='center', fontsize=14, weight='bold')

    # Remove the default colorbar label
    cbar.set_label('')

    # Display the color scale
    plt.savefig('scale.png', bbox_inches='tight', pad_inches=0.05, )
    plt.show()


# Function to compute the pixelwise MSE
def compute_mse(image1_path, image2_path, output_path, colormap='viridis'):
    # Load the two images
    img1 = Image.open(image1_path)
    img2 = Image.open(image2_path)

    # Convert the images to grayscale
    img1_gray = img1.convert("L")  # "L" mode means grayscale
    img2_gray = img2.convert("L")

    # Convert grayscale images to numpy arrays
    img1_array = np.array(img1_gray)
    img2_array = np.array(img2_gray)

    # Compute the pixelwise MSE (Mean Squared Error)
    mse = np.square(img1_array - img2_array)

    # Compute RMSE (Root Mean Squared Error) for each pixel
    mse_image = np.sqrt(mse)
    print(np.max(mse_image))

    # Normalize the RMSE image to the range [0, 1]
    mse_normalized = mse_image / np.max(mse_image)  # Normalize to the range [0, 1]

    # Apply the colormap to the grayscale RMSE image
    colormap = cm.get_cmap(colormap)
    mse_colored = colormap(mse_normalized)  # Apply the color map

    # The result will have 4 channels (RGBA), remove the alpha channel and scale to [0, 255]
    mse_colored_image = (mse_colored[:, :, :3] * 255).astype(np.uint8)  # Keep RGB channels

    # Convert the color-mapped RMSE image back to a PIL image and save it
    mse_img = Image.fromarray(mse_colored_image)
    mse_img.save(output_path)


def plot_error():
    compute_mse("reference99.jpg", "global99.jpg", "error_global.jpg")
    compute_mse("reference99.jpg", "incremental99.jpg", "error_incremental.jpg")
    compute_mse("reference99.jpg", "focusbased99.jpg", "error_focusbased.jpg")
    compute_mse("reference99.jpg", "1spp.jpg", "error_1spp.jpg")
    compute_mse("reference99.jpg", "auto299.jpg", "error_auto2.jpg")


if __name__ == "__main__":
    #plot_scale()
    plot_error()


    globalprio()
    spiral(53,59)
    spiral(43, 58)
    spiral(40, 60)
    double_spiral(53, 59, 10, 10)
    double_spiral(43, 58, 20, 100)
    double_spiral(40, 60, 50, 90)

    # Get the current working directory (root folder of the project)
    root_folder = os.getcwd()

    # Find all .bin files in the root folder
    bin_files = glob.glob(os.path.join(root_folder, 'prioDir*'))

    # Execute the function for each .bin file
    for bin_file in bin_files:
        number, scene = extract_number_from_filename(bin_file)
        import_and_plot_scalar_field(number, scene)
