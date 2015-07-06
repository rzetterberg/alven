local lustache = require "lustache"

pages = kael.get_pages()

data = {
   pages = pages
}

template = kael.read_theme_file("base.mustache")
output   = lustache:render(template, data)

kael.output(output)
