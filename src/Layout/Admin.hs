{-|
Layout rendering functionality for different types of layouts used in the admin
interface. Contains helper functionality for rendering forms.

All layouts are Bootstrap based which means grid units refer to the relative
size of 1 to 12.
-}
module Layout.Admin where

import           Import

-------------------------------------------------------------------------------

{-|
1-column layout with content area of 12 grid units
-}
singleLarge :: Text -> Widget -> Handler Html
singleLarge pageIdentifier content = do
    alertM                    <- getAlertT
    (currCrumb, parentCrumbs) <- breadcrumbs

    buildLayout $ do 
        let alert = $(widgetFile "components/alert")

        $(widgetFile "layout/admin/single_large")

{-|
Renders given form as a
<http://getbootstrap.com/css/#forms-example Bootstrap Basic form>.
-}
renderForm :: AForm Handler a -> Form a
renderForm f = renderBootstrap3 BootstrapBasicForm f

{-|
A boolean field adapted after how Bootstrap structures radio buttons.
See <http://getbootstrap.com/css/#checkboxes-and-radios Checkboxes and radios>.
-}
bs3BoolField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m Bool
bs3BoolField = boolField
      { fieldView = \theId name attrs val isReq -> let attrs' = removeClass "form-control" attrs
                                                   in [whamlet|
$newline never
  $if not isReq
    <div .radio>
      <label>
        <input id=#{theId}-none *{attrs'} type=radio name=#{name} value=none checked>
        _{MsgSelectNone}

<div .radio>
  <label>
    <input id=#{theId}-yes *{attrs'} type=radio name=#{name} value=yes :showVal id val:checked>
    _{MsgBoolYes}

<div .radio>
  <label>
    <input id=#{theId}-no *{attrs'} type=radio name=#{name} value=no :showVal not val:checked>
    _{MsgBoolNo}
|]
    }
  where
    showVal = either (\_ -> False)

{-|
A field that cannot be filled in, only used to display values. See
<http://getbootstrap.com/css/#forms-controls-static Static control>.
-}
bs3StaticTextField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m Text
bs3StaticTextField = textField
    { fieldView = \theId name _ val _ ->
        [whamlet|
$newline never
<input id=#{theId} type=hidden name=#{name} value="#{either id id val}">
<p .form-control-static>#{either id id val}
|]
    }
