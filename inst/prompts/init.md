I'd like to create an app that helps people review data analysis code.

The main prompt is in `inst/prompts/main.md`. Read that first!

It's main interface should be a file editor. Almost like Google Docs (or VS Code's Zen Mode) in that the editor is the only thing visible, in addition to a Docs-style "Comment"/"Review" pane. (There doesn't need to be any sort of Docs "Top bar" interface, e.g. to change fonts or whatever.) 

The `propose_edit` tool should have the following arguments:

* \[ path, insert line, new_str, old_str, as in https://github.com/simonpcouch/side/blob/61998e4fa3a7b21dab2b1cecebaa7c73e6077cdc/R/tool-write_text_file.R \]
* If you do not provide `new_str`, nothing will be shown to the user and your `shift` will be applied, allowing you to move forward in the document without applying any changes.
* `_intent`: A description of the change you're applying, e.g. "Introducing spaces before and after equal signs"
* `justification`: The main interface through which the agent should communicate with the user--this will show up in the comment / revision UI.
* `shift`: A number by which to increment the starting line of the editable region.

Look at `tool_write_text_file()` tool to understand how the first bullets' arguments work.

Notably, however, that tool is situated in a context where an "approval flow" is implemented already. We don't have that. Instead, the tool, instead of returning immediately, will need to wait for the user to Accept, Reject, or provide feedback on the edit.

* The text comments from the Tidy Reviewer will show up on the RHS as if they were in a Google Doc, with their edits to the editable region applied in the editor.
* The default editable region is 1-15, but the UI will just read "Reviewing" with a spinner until the agent proposed an edit. Some UI shows that the "focus" is on a specific set of lines by _very_ slightly dimming content outside of the editable region. Do the dimming with a brief, minimal animation.

For inspiration, here's the "card" for suggestions in Google Docs. We want to lightly mimic this with the "Tidy Reviewer" author name, a blue check mark on the upper RHS, and a grey X to its left. In the 'Replace: "I with "Then, i"' blurb in the image below, show the model's argument for its changes rather than a summary of the change; the summary of the change will be shown in the document via a diff viewer.

plans/comment-ui.png

* The agent is initialized with:
	* The below system prompt
	* The `propose_edit()` tool
	* The first 50 lines of the data analysis file, with the editable region set to 1-15. The lines of the file should be prefixed with the line number, 1-indexed. The editable region will be represented by tags with no line prefix, so a snippet might look like:

```analysis.R
{editable_region}
1: library(ggplot2)
2: library(dplyr)
...
14: group_by(cyl) %>%
15: summarize(
{/editable_region}
16:   mean_mpg = mean(mpg)
17: )
...
```

The main prompt notes <reflection> tags. These will be hidden from the user by shinychat automatically, so no need to do anything with them.
