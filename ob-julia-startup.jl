# Check that required packages are installed:
if Base.find_package("CSV") == nothing || Base.find_package("DelimitedFiles") == nothing
    using Pkg
    Pkg.add("CSV")
    Pkg.add("DelimitedFiles")
end

# Load required packages:
using CSV
using DelimitedFiles

# Perso function to write Julia objects into CSV files:
function ob_julia_write(bodycode, filename, has_header)
    try
        CSV.write(filename, bodycode, delim = "\t", writeheader = has_header)
    catch err
        if isa(err, ArgumentError) | isa(err, MethodError)
            writedlm(filename, bodycode)
        end
    end
end
