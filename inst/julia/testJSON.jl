importExpr = parse("import JSON")
## Because adding a package during initialization can delay startup and cause the waiting process to
## think the connection is blocked, we delay the package add until JSON is first required.
try
    eval(importExpr)
    write(STDOUT, "YES")
catch err
    write(STDOUT, "NO")
end
