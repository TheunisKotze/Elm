module Graphics.Input.Field where
{-| This library specifically addresses text fields. It uses the same general
approach as the [`Graphics.Input`](Graphics-Input) module for describing an
`Input`, so this library focuses on creating and styling text fields.

# Create Fields
@docs field, password, email

# Field Style
@docs Style, Outline, noOutline, Highlight, noHighlight, Dimensions, uniformly
-}

import Color (Color)
import Color
import Graphics.Element (Element)
import Graphics.Input (Input, Handle)
import Native.Graphics.Input
import Text

{-| Easily create uniform dimensions:

      uniformly 4 == { left=4, right=4, top=4, bottom=4 }
-}
uniformly : Int -> Dimensions
uniformly n = Dimensions n n n n

{-| For setting dimensions of a fields padding or border. The left, right, top,
and bottom may all have different sizes.
-}
type Dimensions = { left:Int, right:Int, top:Int, bottom:Int }

{-| A field can have a outline around it. This lets you set its color, width,
and radius. The radius allows you to round the corners of your field. Set the
width to zero to make it invisible.
-}
type Outline = { color:Color, width:Dimensions, radius:Int }

{-| An outline with zero width, so you cannot see it. -}
noOutline : Outline
noOutline = Outline Color.grey (uniformly 0) 0

{-| When a field is selected, it has an highlight around it by default. Set the
width of the `Highlight` to zero to make it go away.
-}
type Highlight = { color:Color, width:Int }

{-| An highlight with zero width, so you cannot see it. -}
noHighlight : Highlight
noHighlight = Highlight Color.blue 0

{-| Describes the style of a text box. The `style` field describes the style
of the text itself. The `outline` field describes the glowing blue outline that
shows up when the field has focus. Turn off `outline` by setting its width to
zero. The 
-}
type Style =
  { padding   : Dimensions
  , outline   : Outline
  , highlight : Highlight
  , style     : Text.Style
  }

{-| The default style for a text field. The outline is `Color.grey` with width
1 and radius 2. The highlight is `Color.blue` with width 1, and the default
text color is black.
-}
defaultStyle : Style
defaultStyle =
  { padding   = uniformly 4
  , outline   = Outline Color.grey (uniformly 1) 2
  , highlight = Highlight Color.blue 1
  , style     = Text.defaultStyle
  }

{-| Create a text field. The following example creates a time-varying element
called `nameField`. As the user types their name, the field will be updated
to match what they have entered.

      name : Input String
      name = input ""

      nameField : Signal Element
      nameField = field name.handle id defaultStyle "Name" <~ name.signal
-}
field : Handle a -> (String -> a) -> Style -> String -> String -> Element
field = Native.Graphics.Input.field

{-| Same as `field` but the UI element blocks out each characters. -}
password : Handle a -> (String -> a) -> Style -> String -> String -> Element
password = Native.Graphics.Input.password

{-| Same as `field` but it adds an annotation that this field is for email
addresses. This is helpful for auto-complete and for mobile users who may
get a custom keyboard with an `@` and `.com` button.
-}
email : Handle a -> (String -> a) -> Style -> String -> String -> Element
email = Native.Graphics.Input.email

-- area : Handle a -> (String -> a) -> Handle b -> ((Int,Int) -> b) -> (Int,Int) -> String -> String -> Element
-- area = Native.Graphics.Input.area