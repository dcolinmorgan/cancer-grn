[
   {"selector":"node", "css": {
       "text-valign":"center",
       "text-halign":"center",
       "border-color": "black",
       "content": "data(name)",
       "border-width": "1px",
       "width": "mapData(degree, 0, 20, 50, 100)",
       "height": "mapData(degree, 0, 20, 50, 100)"
       }},

    {"selector": "node[rank<=0.5]", "css": {
        "background-color": "mapData(rank, 0, 0.5, red, white)"
        }},

    {"selector": "node[rank>0.5]", "css": {
        "background-color": "mapData(rank, 0.50001, 1, white, blue)"
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
        "target-arrow-shape": "triangle",
        "target-arrow-color": "blue",
        "arrow-scale": 3

    }},

    {"selector": "edge[interaction='inhibit']", "css": {
        "line-color": "red",
        "target-arrow-shape": "tee",
        "target-arrow-color": "red",
        "arrow-scale": 3

      }}
]