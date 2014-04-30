# All functions related to traversal of file system for grabbing Syberia related files
#
# By convention, the structure from a syberia root project will look like this:
# - data    # Data preparation for data sources coming from an external API
#   - data_source1
#     - data_source1.r
#     - helpers.r
#   - data_source2.r 
# - models  # Files that parametrize the modeling process for each model version
#   - dev   # The development environment to be used as a sandbox
#   - prod  # Models will be copied over from dev to prod for deployment
#   - test  # Unit tests for the models - should be environment-agnostic (dev or prod)
# - syberia.config  # Configuration file for syberia project

