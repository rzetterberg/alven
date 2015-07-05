pages = kael.get_pages()

kael.output("<ul>")

for k,v in pairs(pages) do
   kael.output("<li>")
   kael.output(v["name"])
   kael.output("</li>")
end

kael.output("</ul>")
