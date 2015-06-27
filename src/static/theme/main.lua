local lustache = require "lustache"

page = get_current_page()

if page == nil then
   page = { title = "Not found",
            body  = "The page could not be found"
   }
end

pages = get_pages()

data = {
   page  = page,
   pages = pages
}

template = read_theme_file("base.mustache")
output   = lustache:render(template, data)

print(output)
