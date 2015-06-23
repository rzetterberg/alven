from selenium import webdriver  
import os
  
def before_all(context):  
    context.base_url = os.environ['BUILDER_PORT'].replace("tcp:", "http:")
    context.browser  = webdriver.PhantomJS()

    context.browser.set_window_size(1024, 768)

  
def after_all(context):  
    context.browser.quit()  
