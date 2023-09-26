# Machine learning and statistical learning
# K-Nearest Neighbors

# Author Jean-Baptiste GOMEZ

# 1 Context
# In this exercise, you will build a classifier using the K-Nearest Neighbors algorithm.



# 2 Lab setup: generating data
# In a first step, you will generate data. To do so:
#   1. Draw n = 50 observations in a unit square. To do so:
#   . randomly generate 50 observations from a Beta distribution with parameters ?? =
#   ?? = 1 and store the drawn values in an object you will call x.

# Set the seed for reproducibility (optional)
set.seed(12345)
n <- 50
x <- rbeta(n, shape1 = 1, shape2 = 1)
print(x)


# . Do the same procedure and store the draws in an object you will call y.

set.seed(6789)
n <- 50
y <- rbeta(n, shape1 = 1, shape2 = 1)
print(y)

# 2. Create a vector you will call true_label of size n = 50 which will contain the true labels:
#   "orange" or "blue".
# . "orange" if x + y ??? 1
# . "blue" otherwise.

true_label <- ifelse(x + y >= 1, "orange", "blue")
print(true_label)

# Anther method
# Create an empty vector to store true labels
# true_label <- character(0)
# 
# for (i in 1:length(x)) {
#   if (x[i] + y[i] >= 1) {
#     true_label <- c(true_label, "orange")
#   } else {
#     true_label <- c(true_label, "blue")
#   }
# }
# 
# print(true_label)
# Calculate the true_label without a loop



# 3. Create a new point (x0, y0) at which you will try to assign a label, depending on the values
# of the nearest neighbors. For example: (x0 = 0.75, y0 = 0.5).

x0 <- 0.75
y0 <- 0.5

# Number of nearest neighbors to consider
k <- 5

# Calculate the Euclidean distances between (x0, y0) and all data points
distances <- sqrt((x - x0)^2 + (y - y0)^2)
print(distances)

# Get the indices of the k nearest neighbors in ascending order
nearest_indices <- order(distances)[1:k]
print(nearest_indices)

# Extract the true labels of the nearest neighbors
nearest_labels <- true_label[nearest_indices]

# Count the occurrences of each label
label_counts <- table(nearest_labels)
print(label_counts)


# Find the label with the highest count (mode)
predicted_label <- names(which.max(label_counts))

# Print the predicted label for (x0, y0)
cat("Predicted Label for (x0, y0):", predicted_label, "\n")


# 4. Create a matrix with 3 columns: the x and y coordinates of your generated points, and
# the assigned label.

data_matrix <- cbind(x, y, true_label)
print(data_matrix)


# 5. Plot your 50 observations on a scatter plot and add the new (x0, y0) observation using a
# different color/shape.

# Define the new point coordinates
x0 <- 0.75
y0 <- 0.5

# Create a scatter plot for the 50 observations
plot(x, y, col = ifelse(true_label == "orange", "orange", "blue"), pch = 19, main = "Scatter Plot with new x0 y0")

# Add the new point (x0, y0) with a different color and shape
points(x0, y0, col = "green", pch = 2, cex = 2)

# Add labels for the legend
# legend("topright", legend = c("orange", "blue", "New Point"), col = c("orange", "blue", "green"), pch = c(19, 19, 2))


# 3 The algorithm

# 1. To know which are the K closests points of your new observation, you need to compute
# the distance between each point of your dataset and your new observation. To that end,
# create a function that computes the distances between two points:
# . this function will require four parameters: the two coordinates of a first point (xA and yA) and the two coordinates of a second point (xB and yB).
# . it will return the Euclidean distance between the two points whose coordinates are
# given as parameters.

# Function to compute Euclidean distance between two points
euclidean_distance <- function(xA, yA, xB, yB) {
  xA <- as.numeric(xA)
  yA <- as.numeric(yA)
  xB <- as.numeric(xB)
  yB <- as.numeric(yB)
  
  distance <- sqrt((xA - xB)^2 + (yA - yB)^2)
  return(distance)
}

# Test the function with example points
xA <- 1
yA <- 2
xB <- 3
yB <- 4

distance <- euclidean_distance(xA, yA, xB, yB)
cat("Euclidean Distance:", distance, "\n")




# 2. Using a loop, apply this function to your new point (x0, y0) and each of the points in your
# dataset. In other words, at iteration i, store the Euclidean distance between your point
# (x0, y0) and the i-th point from your data, i.e., (xi, yi). Once you have computed the
# distance from your point (x0, y0) to all points from your dataset, order your dataset by
# increasing distances to your new point.

# Compute and store distances between (x0, y0) and each point in the dataset
distances <- numeric(nrow(data_matrix))
for (i in 1:nrow(data_matrix)) {
  distances[i] <- euclidean_distance(x0, y0, data_matrix[i, 1], data_matrix[i, 2])
}

data_matrix <- cbind(data_matrix, Distance = distances)
ordered_data <- data_matrix[order(distances), ]
print(ordered_data)




# 3. Pick a value for K. For example, K = 3.
# 4. In a new object, copy the K first rows of your dataset that was previously ordered by
# ascending values of the distance to the new point: this allows you to keep the K nearest
# neighbors.

K <- 3
nearest_neighbors <- ordered_data[1:K, ]
print(nearest_neighbors)


# 5. Plot the points of this dataset in a different color.

plot(x, y, col = ifelse(true_label == "orange", "orange", "blue"), pch = 19, main = "Scatter Plot with New x0 y0")
# Add the new point (x0, y0) with a different color and shape
points(x0, y0, col = "green", pch = 2, cex = 2)
# Highlight the K nearest neighbors in red
points(nearest_neighbors[, 1], nearest_neighbors[, 2], col = "red", pch = 19)
# legend("topright", legend = c("orange", "blue", "New Point", "K Nearest Neighbors"), col = c("orange", "blue", "green", "red"), pch = c(19, 19, 2, 19))


# 6. Based on that dataset with only the K nearest neighbors, compute the number of "blue"
# and the number of "orange", then provide an estimation of the probability for the new
# observation to be blue.
k<-3
nearest_neighbors <- as.data.frame(ordered_data[1:K, ])
# Calculate the number of "blue" and "orange" points among the K nearest neighbors
blue_count <- sum(nearest_neighbors$true_label == "blue")
orange_count <- sum(nearest_neighbors$true_label == "orange")

# Calculate the estimated probability for the new observation to be "blue"
estimated_probability_blue <- blue_count / K
estimated_probability_orange <- orange_count / K

# Print the counts and estimated probability
cat("Number of 'blue' points among K nearest neighbors:", blue_count, "\n")
cat("Number of 'orange' points among K nearest neighbors:", orange_count, "\n")
cat("Estimated probability for the new observation to be 'blue':", estimated_probability_blue, "\n")


# 7. Based on that probability, assign a predicted class to your new observation.

# estimated_probability_blue <- 0.4  # Replace with the actual estimated probability
# predicted_class <- ifelse(estimated_probability_blue >= 0.5, "blue", "orange")
# cat("Predicted Class for the new observation:", predicted_class, "\n")

predicted_class <- ifelse(estimated_probability_orange > estimated_probability_blue, "orange", "blue")
cat("Predicted Class for the new observation:", predicted_class, "\n")

# 8. Set a different value for K and look at how it may change your prediction.

K <- 15
nearest_neighbors <- as.data.frame(ordered_data[1:K, ])

# Calculate the number of "blue" and "orange" points among the K nearest neighbors
blue_count <- sum(nearest_neighbors$true_label == "blue")
orange_count <- sum(nearest_neighbors$true_label == "orange")

# Calculate the estimated probability for the new observation to be "blue"
estimated_probability_blue <- blue_count / K

# Assign a predicted class based on the estimated probability
predicted_class <- ifelse(estimated_probability_blue >= 0.5, "blue", "orange")

# Print the results
cat("For K =", K, "\n")
cat("Number of 'blue' points among K nearest neighbors:", blue_count, "\n")
cat("Number of 'orange' points among K nearest neighbors:", orange_count, "\n")
cat("Estimated probability for the new observation to be 'blue':", estimated_probability_blue, "\n")
cat("Predicted Class for the new observation:", predicted_class, "\n")








# Define a sequence of K values to test
K_values <- seq(1, 50, by = 1)  # Adjust the range as needed

# Initialize variables to store results
max_probability <- 0
optimal_K <- 0

# Iterate through the K values
for (K in K_values) {
  # Create a new object containing the K nearest neighbors as a data frame
  nearest_neighbors <- as.data.frame(ordered_data[1:K, ])
  
  blue_count <- sum(nearest_neighbors$true_label == "blue")
  orange_count <- sum(nearest_neighbors$true_label == "orange")
  
  
  # Calculate the estimated probability for the new observation to be "blue"
  estimated_probability_blue <- blue_count / K
  # predicted_class <- ifelse(estimated_probability_blue >= 0.5, "blue", "orange")
  
  # Check if the current estimated probability is greater than the previous maximum
  if (estimated_probability_blue > max_probability) {
    max_probability <- estimated_probability_blue
    optimal_K <- K
  }
  
  # Print results for each K
  cat("For K =", K, "\n")
  cat("Number of 'blue' points among K nearest neighbors:", blue_count, "\n")
  cat("Estimated probability for the new observation to be 'blue':", estimated_probability_blue, "\n\n")
}

# Print the K with the maximum probability
cat("Optimal K for Maximum Probability:", optimal_K, "\n")
cat("Maximum Probability:", max_probability, "\n")












