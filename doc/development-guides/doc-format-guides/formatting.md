### Formatting Documentation 

> Cheat Sheet: http://warpedvisions.org/projects/markdown-cheat-sheet/

#### Links

You can create links to pages using with square braces for the link text and parens for the link address.

`[Link Contents](Link URL)`

For relative links, use the doc name with the extension.  You can navigate the docs tree using relative paths, e.g.: ../to_parent.   For example, [sibling doc](topic.md).


#### WYSIWG Editing

Markdown is designed to be easy to ready as plan text (think writing an email) but it really helps to be able to see how it will be formatted in the browser.

We recommending using [[http://www.ctrlshift.net/project/markdowneditor/]] to WYSIWYG edit markdown!

Of course, you'll have to save it back to the source document after you browse.

#### Special Characters

If you want to use special charcters, just include them in the text w/ the HTML escape code.  For example, Trademark &trade 
http://www.escapecodes.info/

#### HTML Styling (considered harmful)

If you must use HTML (like for tables) then avoid adding any style or formatting information.  Assume that we'll apply CSS to the output so the look is consistent.

#### Images

You can include images in your documentation using `![caption](image_name.png)`

This will create an HTML image in a paragraph.

Put the image in the same directory and Crowbar will make sure the relative paths work.

> Please put your source material for the image in the directory too so that future editors can update images as needed.

#### Tables

For tables, formatting them using HTML with a lot of white space (see below).  While not ideal, it makes it pretty easy to edit and read

  <table>
    <tr>
      <td>this is cell 1</td>
      <td>this is cell2</td>
    </tr>
  </table>

