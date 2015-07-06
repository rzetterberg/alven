local lustache = require "lustache"

pages = alven.get_pages()

data = {
   pages = pages
}

template = alven.read_theme_file("base.mustache")
output   = lustache:render(template, data)

alven.output(output)
