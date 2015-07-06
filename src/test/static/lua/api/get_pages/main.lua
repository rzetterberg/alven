pages = alven.get_pages()

pages_len = 0

for k, v in pairs(pages) do
  pages_len = pages_len + 1
end

alven.output(tostring(pages_len))
