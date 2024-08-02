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


if __name__ == "__main__":
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
