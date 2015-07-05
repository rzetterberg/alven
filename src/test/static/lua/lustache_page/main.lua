local lustache = require "lustache"

page = kael.get_current_page()

if page == nil then
   page = { title = "Not found",
            body  = "The page could not be found"
   }
end

pages = kael.get_pages()

data = {
   page  = page,
   pages = pages
}

template = kael.read_theme_file("base.mustache")
output   = lustache:render(template, data)

kael.output(output)
