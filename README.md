# purify_css


Remove unused rules of a css file, for a set of rules to remove stored in a file or a character vector.

To obtain the unused rules, this is a possible approach:

- Go to google Chrome, Open Properties > Tools > Developer Tools

- Click on the Audits tab within the Developer Tools window

- Ensure the Web Page Performance option is checked.

- Click Run

- Copy into a txt file the unused css rules that the audit tool returns 

</br> </br>

- **Now run this tool**:


###**purify_css(css_file, unused_rules_file)**
</br> </br>


----An example ----

With the sample files my_css.css and unused-rules.txt, type in R:


purify_css("/directory_with_files/my_css.css", "/directory_with_files/unused_rules_file")


The clean file, having the same name of your css file followed by the suffix "out.css", will be created in the directory

--------o---------


