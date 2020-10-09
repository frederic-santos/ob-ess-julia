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
function my_csv_write(filename, bodycode, has_header)
    CSV.write(filename, bodycode, delim = "\t", writeheader = has_header)
    nothing
end

function objuliawrite(bodycode::Any, filename::Any, has_header::Any)
    try
        my_csv_write(filename, bodycode, has_header);
    catch err
        if isa(err, ArgumentError) | isa(err, MethodError)
            writedlm(filename, bodycode)
        end
    end
end
