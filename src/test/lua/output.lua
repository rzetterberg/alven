local lustache = require "lustache"

p = get_current_page()

output = lustache:render("<h1>{{name}}</h1>{{&body}}", p)

print(output)
