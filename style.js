[
   {"selector":"node", "css": {
       "text-valign":"center",
       "text-halign":"center",
       "border-color": "black",
       "content": "data(name)",
       "border-width": "1px",
       "font-size":"10px",
       "width": "40px",
       "height": "40px"
       }},

    {"selector": "node[rank>0]", "css": {
        "background-color": "mapData(rank, 0, 1, white,cyan)"
    }},

    {"selector": "node:selected", "css": {
       "overlay-opacity": 0.3,
       "overlay-color": "gray"
    }},

    {"selector": "edge", "css": {
        "curve-style": "bezier"
    }},
{"selector": "edge[interaction='stimulate']", "css": {
        "line-color": "blue",
        "width": 1,
        "target-arrow-shape": "triangle",
        "target-arrow-color": "blue",
        "arrow-scale": 1
    }},

    {"selector": "edge[interaction='inhibit']", "css": {
        "line-color": "red",
        "width": 1,
        "target-arrow-shape": "tee",
        "target-arrow-color": "red",
        "arrow-scale": 1
      }}
]
