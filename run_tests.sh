#!/bin/bash

# Initialize total time to zero
total_time=0

# Run the test suite 10 times
for i in {1..10}
do
  # Capture the start time in nanoseconds
  start_time=$(date +%s%N)
  
  # Run the test suite
  stack test :valid-types-quick
  
  # Capture the end time in nanoseconds
  end_time=$(date +%s%N)
  
  # Calculate the duration for this run in nanoseconds
  duration=$((end_time - start_time))
  
  # Convert the duration to seconds (since nanoseconds are too granular)
  duration_sec=$(awk "BEGIN {print $duration/1000000000}")
  
  # Add the duration to the total time
  total_time=$(awk "BEGIN {print $total_time + $duration_sec}")
  
  # Output the duration for this run
  echo "Run $i: $duration_sec seconds"
done

# Calculate the average duration
average_time=$(awk "BEGIN {print $total_time / 10}")

# Output the average duration
echo "Average time: $average_time seconds"
