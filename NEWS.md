## factordist 0.1.2
---------------------
* Add `as_consensus_cluster` function to suggest final labels from consensus matrix

## factordist 0.1.1
---------------------
* Set `strict = TRUE` as default `as_consensus_matrix` behavior
* Update `mode` to ignore NAs

## factordist 0.1.0
---------------------
* Add `as_consensus_matrix` function to repeat `match_labels` across matrix columns
* Clean up file structure and documentation
* Changes to streamline backend
    * Rename `ready_data` to `as_factor_frame`
    * New `resample` to avoid the `sample(N)` bug
    * New `which_max` and `which_min` to return sample max/min index
    * New `args_as_list` and `do_call` to simplify calling

## factordist 0.0.11
---------------------
* Add `match_labels` function to match cluster assignments
* Update `s_relAcc`
    * Fix denominator to ignore double NAs more precisely

## factordist 0.0.10
---------------------
* Add `s_relAcc` function to compute relative accuracy

## factordist 0.0.9
---------------------
* Add unit test for `sym` functions

## factordist 0.0.8
---------------------
* Fix performance bottleneck in `predict.cluster_model` method

## factordist 0.0.7
---------------------
* Add `circularize` function

## factordist 0.0.6
---------------------
* Update `cluster_model`
    * Pre-process `factordist` and `cluster_model` input the same way
* Update `predict.cluster_model`
    * Pre-process `factordist` and `predict.cluster_model` input the same way
    * Add progress bar
* General changes
    * Use `inherits` for class checks

## factordist 0.0.5
---------------------
* Update `factordist`
    * Coerce input into factors with the same levels
    * Alert user if input contains NA values
* Update `cluster_model`
    * When x and y are data.frames, check that row names match

## factordist 0.0.4
---------------------
* Update `predict.cluster_model`
    * Assign NA label when there are no nearest neighbors
    * Now handles asymmetric difference functions
    * Now handles character labels

## factordist 0.0.3
---------------------
* Add `cluster_model` class and methods

## factordist 0.0.2
---------------------
* Add `s_union`, a set overlap similarity metric
* Add `d_jaccard`, a set overlap difference metric
* Update `factordist` front-end for Jaccard

## factordist 0.0.1
---------------------
* Add `factordist` front-end function
* Add `papply` to standardize difference function deployment
* Add `d_adjRand`, a meta-clustering difference metric
