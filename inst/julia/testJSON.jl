using Pkg

function addJSON()
    try
        write(stderr, "Trying to add Julia package JSON; expect some messages and some delay\n")
        Pkg.add("JSON")
        0
    catch err
        write(stderr, "Unable to add package JSON: ")
        showerror(stderr, err)
        1
    end
end

try
    import JSON
    exit(0)
catch err
    exit(addJSON())
end
