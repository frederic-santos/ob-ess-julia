# SPDX-License-Identifier: CECILL-2.1

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

# Specific Windows stuff:
# (Contribution on GitHub by @fkgruber:
# https://github.com/frederic-santos/ob-ess-julia/issues/1#issuecomment-707433134
# )
if Sys.iswindows()
    println("In startup script")
    using Base: stdin, stdout, stderr
    using REPL.Terminals: TTYTerminal
    using REPL: BasicREPL, run_repl
    using DelimitedFiles
    run_repl(BasicREPL(TTYTerminal("emacs",stdin,stdout,stderr)))
    println("Finished initial startup")
end
