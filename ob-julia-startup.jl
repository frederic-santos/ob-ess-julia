# Check that required packages are installed:
if Base.find_package("CSV") == nothing | Base.find_package("DelimitedFiles") == nothing
    using Pkg
    Pkg.add("CSV")
    Pkg.add("DelimitedFiles")
end

# Load required packages:
using CSV
using DelimitedFiles
