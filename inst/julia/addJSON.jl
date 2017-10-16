## import JSON, adding package the first time if not available.
## Julia's try statement only works on function calls, so a kludge is needed
## to put the import statement into a try
importExpr = parse("import JSON")
try
    write(STDERR, "Trying to add Julia package JSON; expect some messages and some delay\n")
    Pkg.add("JSON")
    eval(importExpr)
    true
catch err
    write(STDERR, "Unable to add and import JSON: ")
    showerror(STDERR, err)
    false
end
