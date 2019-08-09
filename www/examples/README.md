## Creating Examples

To create a new example, run ShinyDAG locally using the shinydag dev docker file.

Set up your example and then save it as a bookmark.

Note the URL created by shiny for the bookmark, it should end with

```
?_state_id_=9d49cb0ba72b00f2
```

Navigate to the `shiny_bookmarks` folder and find the folder with the bookmark token, e.g. `9d49cb0ba72b00f2`.

Copy `values.rds` to `www/examples` and give the file a descriptive name.
These names are used for the shiny inputs, so keep the characters sane (and no spaces).

Also, save the DAG image into `www/examples` with the same name (not required but a good idea).

Finally, add the description text to `www/examples/examples.yml`.
Here's an example template that you can copy.
Note that if you can use HTML in the `description`, but it needs to be valid or it will cause problems on the page.

```yaml
- name: Classic Confounding
  description: >
    This is a description of classic confounding. Descriptions may include
    <strong>HTML</strong>.
  file: classic-confounding.rds
  image: classic-confounding.png
```