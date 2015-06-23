local lustache = require "lustache"

page     = get_current_page()
template = load_template("test/lua/base.mustache")
output   = lustache:render(template, page)

print(output)
