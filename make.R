# readd(<target name>): Reads the target object from the cache
# loadd(): Load all targets from drake
# r_outdated(): Determines which targets in your plan are outdated
# r_vis_drake_graph(): Renders and displays the state of the drake pipeline
# r_sankey_drake_graph(): A different style of network graph

library(drake)

r_make()
r_vis_drake_graph()
r_sankey_drake_graph()
r_outdated()

loadd()
