import numpy as np
import matplotlib.pyplot as plt
import os
import glob
import re


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

    prioComb = prioDir + prioInd

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


if __name__ == "__main__":
    # Get the current working directory (root folder of the project)
    root_folder = os.getcwd()

    # Find all .bin files in the root folder
    bin_files = glob.glob(os.path.join(root_folder, 'prioDir*'))

    # Execute the function for each .bin file
    for bin_file in bin_files:
        number, scene = extract_number_from_filename(bin_file)
        import_and_plot_scalar_field(number, scene)
