module Layout.Admin where

import           Import

-------------------------------------------------------------------------------

singleLarge :: Text -> Widget -> Handler Html
singleLarge pageIdentifier content = do
    alertM                    <- getAlertT
    (currCrumb, parentCrumbs) <- breadcrumbs

    buildLayout $ do 
        let alert = $(widgetFile "components/alert")

        $(widgetFile "layout/admin/single_large")

renderForm :: AForm Handler a -> Form a
renderForm f = renderBootstrap3 BootstrapBasicForm f

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

bs3StaticTextField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m Text
bs3StaticTextField = textField
    { fieldView = \theId name _ val _ ->
        [whamlet|
$newline never
<input id=#{theId} type=hidden name=#{name} value="#{either id id val}">
<p .form-control-static>#{either id id val}
|]
    }
