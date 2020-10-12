# Check that required packages are installed:
if Base.find_package("CSV") == nothing || Base.find_package("DelimitedFiles") == nothing || Base.find_package("Pipe") == nothing
    using Pkg
    Pkg.add("CSV")
    Pkg.add("DelimitedFiles")
    Pkg.add("Pipe")
end

# Load required packages:
using CSV
using DelimitedFiles
using Pipe

# Perso function to write Julia objects into CSV files:
function ob_ess_julia_csv_write(filename, bodycode, has_header)
    CSV.write(filename, bodycode, delim = "\t", writeheader = has_header);
end

function ob_ess_julia_write(bodycode::Any, filename::Any, has_header::Any)
    try
        ob_ess_julia_csv_write(filename, bodycode, has_header);
    catch err
        if isa(err, ArgumentError) | isa(err, MethodError)
            writedlm(filename, bodycode)
        end
    end
end
