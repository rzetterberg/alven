page = alven.get_current_page()

if page == nil then
   alven.output("")
else
   alven.output(page["name"])
end
