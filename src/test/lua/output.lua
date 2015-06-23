print("Sending data back to haskell\n")
print("Hello!\n")
print("How are you today?")

p = get_current_page()

print(p["name"])
print(p["body"])

local lustache = require "lustache"

view_model = {
  title = "Joe",
  calc = function ()
    return 2 + 4
  end
}

output = lustache:render("{{title}} spends {{calc}}", view_model)

print(output)
