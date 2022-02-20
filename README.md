# reflex-dom-th
Do you develop for the web? And you know functional reactive programming is the way to go. So you must use Reflex-DOM.
But how can you integrate all these HTML snippet, which you found. You are tired in converting everything to to el, elAttr' etc, right?

From this day on this reflex-dom-th library will automate the task of converting your HTML templates to Reflex-Dom.

# Examples

The basic example

```
[dom|<div>hello</div>|]
```

is equivalent to

```
el "div" $ text "hello"
```

You can also put the html in a external file and include it with

```
$(domFile "template.html")
```

It it possible to have multiple elements and attributes

```
[dom|<ul class="list">
       <li>Item1</div>
       <li>Item1</div>
     </ul> |]
```     

Dynamic content can be injected between two curly braces. It will reference an unbound variable. It is not a haskell expression. Keeping haskell out of the template will give you better error messages.

```
[dom|<ul class="list">
      <li>{{item1}}</div>
      <li>{{item2}}</div>
    </ul> |]
  where item1, item2 :: MonadWidget t m =>  m ()
        item1 = text "Item1"
        item2 = text "Item2"
```        


To bind events to the elements it is possible to extract get the elements as a result. The reference number is the position in the result tuple.

```
(li1, li2, ul, w) <- [dom|<ul #2 class="list">
                           <li #0>Item1</div>
                           <li #1>Item1</div>
	                   <li>{{widget #3}}</div>
                          </ul> |]
```		       

