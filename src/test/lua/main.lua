local lustache = require "lustache"

page = get_current_page()

if page == nil then
   page = { title = "Not found",
            body  = "The page could not be found"
   }
end
   
template = load_template("base.mustache")
output   = lustache:render(template, page)

print(output)
