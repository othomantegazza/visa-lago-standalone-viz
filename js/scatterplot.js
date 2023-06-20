// Copyright 2021 Observable, Inc.
// Released under the ISC license.
// https://observablehq.com/@d3/scatterplot
function Scatterplot(data, {
    x = ([x]) => x, // given d in data, returns the (quantitative) x-value
    y = ([y]) => y, // given d in data, returns the (quantitative) y-value
    area = ([area]) => area, // given d in data, returns the (quantitative) radius, mapping the value to the area
    fitted = ([fitted]) => fitted,
    fill = ([fill]) => fill,
    id = ([id]) => id,
    label = ([label]) => label,
    marginTop = 40, // top margin, in pixels
    marginRight = 10, // right margin, in pixels
    marginBottom = 50, // bottom margin, in pixels
    marginLeft = 40, // left margin, in pixels
    inset = 0, // inset the default range, in pixels
    insetTop = inset, // inset the default y-range
    insetRight = inset, // inset the default x-range
    insetBottom = 0, // inset the default y-range
    insetLeft = inset, // inset the default x-range
    width = 640, // outer width, in pixels
    height = 400, // outer height, in pixels
    minWidth = 450,
    maxWidth = 1000,
    xType = d3.scaleLinear, // type of x-scale
    xDomain, // [xmin, xmax]
    xLabel = "GDP per capita from the previous year, in equivalent US dollars [$] →", // a label for the x-axis
    xFormat, // a format specifier string for the x-axis
    yType = d3.scaleLinear, // type of y-scale
    yDomain, // [ymin, ymax]
    yLabel = "↑ Percent of Application Rejected [%]", // a label for the y-axis
    yFormat, // a format specifier string for the y-axis
    rType = d3.scaleLinear,
    rDomain,
    rRange = [0, 15],
    fillPalette, // an object with named colour
    fillOpacity = 0.9,
    fontSize = 14,
    strokeWidth = 2.5, // stroke width for dots
    guideSizeBreaks = [10000, 100000, 500000, 1000000],
    voronoiShow = false,
} = {}) {

    function radius_from_area(A) {
        return Math.sqrt(A / Math.PI)
    }

    // var test = [1, 222, 22]
    // var result = Array.from(test.keys()).sort(function(a, b) {return test[a]-test[b]})

    function orderIndex(v) {
        const result = Array.from(v.keys()).sort(function (a, b) { return v[a] - v[b] })
        return (result)
    }

    // Compute page layout values
    if (width < minWidth) {
        width = minWidth
    }
    var xoffset = 0
    if (width > maxWidth) {
        width = maxWidth
    }

    // function  that formats y axis text
    const formatPercent = d3.format(".0%")

    // Define scales parameters and build data variables
    const xRange = [marginLeft + insetLeft, width - marginRight - insetRight] // [left, right]
    const yRange = [height - marginBottom - insetBottom, marginTop + insetTop] // [bottom, top]
    const X = d3.map(data, x)
    const Y = d3.map(data, y)
    const AREA = d3.map(data, area)
    const R = d3.map(AREA, radius_from_area)
    const FILL = d3.map(data, fill)
    const FITTED = d3.map(data, fitted)
    const ID = d3.map(data, id)
    const LABEL = d3.map(data, label)
    const I = d3.range(data.length)
    let ORDER = orderIndex(X)

    // Compute default domains.
    if (xDomain === undefined) xDomain = [0, d3.max(X) + d3.max(X) * 0.02]
    if (yDomain === undefined) yDomain = [0, d3.max(Y) + d3.max(Y) * 0.02]
    if (rDomain === undefined) rDomain = [0, d3.max(R)]

    // Construct scales and axes.
    const xScale = xType(xDomain, xRange)
    const yScale = yType(yDomain, yRange)
    const rScale = rType(rDomain, rRange)
    const xAxis = d3.axisBottom(xScale).ticks(width / 100, xFormat)
    const yAxis = d3.axisLeft(yScale).ticks(height / 100, yFormat).tickFormat(formatPercent)

    // line generator function
    line = d3.line()
        .x(i => xScale(X[i]))
        .y(i => yScale(FITTED[i]))

    // voronoi generator
    const dataForVoronoi = d3.map(I, i => [xScale(X[i]), yScale(Y[i])])
    const voronoiRange = [xRange[0], yRange[1], xRange[1], yRange[0]]
    const voronoi = d3.Delaunay
        .from(dataForVoronoi)
        .voronoi(voronoiRange);

    console.log({FILL, fillPalette})
    // console.log({ dataForVoronoi, voronoiShow, render: voronoi.render(), renderCell: voronoi.renderCell(1) })

    // console.log({
    //     x, y,
    //     xRange, yRange,
    //     X, Y, I,
    //     xDomain, yDomain,
    //     FILL, FITTED, fillPalette, ORDER,
    //     America: fillPalette["Europe"],
    //     voronoi,
    //     ID, LABEL
    // })

    // generate tooltip
    const tooltip = d3.select("body")
        .append("div")
        .style("position", "absolute")
        .attr("id", "scatter-tooltip")
        .style("transition", "0.1s")

    // generate SVG
    const svg = d3.create("svg")
        .attr("width", width)
        .attr("height", height)
        .attr("viewBox", [0, 0, width, height])
        .attr("id", "svgscatter")
        .attr("style", `max-width: 100%`)
        .attr("style", "cursor: crosshair; margin: auto; display: block;")

    // axis x                  
    svg.append("g")
        .attr("transform", `translate(0,${height - marginBottom})`)
        .attr("class", "xaxis")
        .call(xAxis)
        .call(g => g.selectAll(".tick line").clone()
            .attr("y2", marginTop + marginBottom - height)
            .attr("stroke-opacity", 0.2)
            .attr("stroke-width", "0.5px"))
        .call(g => g.selectAll(".tick text")
            .attr("font-size", fontSize))
        .call(g => g.append("text")
            .attr("x", width)
            .attr("y", marginBottom - 4)
            .attr("font-size", fontSize)
            .attr("fill", "currentColor")
            .attr("text-anchor", "end")
            .text(xLabel));

    // axis y
    svg.append("g")
        .attr("transform", `translate(${marginLeft},0)`)
        .attr("class", "yaxis")
        .call(yAxis)
        .call(g => g.selectAll(".tick line").clone()
            .attr("x2", width - marginLeft - marginRight)
            .attr("stroke-opacity", 0.2)
            .attr("stroke-width", "0.5px"))
        .call(g => g.selectAll(".tick text")
            .attr("font-size", fontSize)
            .attr("dy", 0.1))
        .call(g => g.append("text")
            .attr("x", -marginLeft)
            .attr("y", fontSize)
            .attr("fill", "currentColor")
            .attr("text-anchor", "start")
            .attr("font-size", fontSize)
            .text(yLabel))

    // line from fitted values
    svg.append("g")
        .append("path")
        .attr("fill", 'none')
        .attr("stroke", 'black')
        .attr("stroke-dasharray", "6 2")
        .attr("d", line(ORDER));

    // circles    
    svg.append("g")
        .attr("stroke-width", strokeWidth)
        .selectAll("circle")
        .data(I)
        .join("circle")
        .attr("cx", i => xScale(X[i]))
        .attr("cy", i => yScale(Y[i]))
        .attr("r", i => rScale(R[i]))
        .attr("fill", i => fillPalette[FILL[i]])
        .attr("fill-opacity", fillOpacity)
        .attr("id", i => ID[i])

        // voronoi grid
        if (voronoiShow) {
            var voronoiStroke = '#00000088'
        } else {
            var voronoiStroke = 'none'
        }
        svg.append("g")
        .attr("stroke", voronoiStroke)
        .attr("fill", "none")
        .selectAll("path")
        .data(I)
        .join("path")
        .attr("d", i => voronoi.renderCell(i))
        .attr("id", i => ID[i])
        .style("pointer-events", "all")
        .on("mousemove touchstart", (e) => mousemove(e))
        

    // Guides
    const fillKeys = Object.keys(fillPalette)
    function guideY(i) {
        return yScale(yDomain[1] * 0.9) + fillKeys.indexOf(i) * 30
    }
    function sizeGuideCircleY(i, mult) {
        const y = guideY(fillKeys.at(-1)) + guideSizeYOffset - rScale(radius_from_area(i)*mult)
        return y
    }
    console.log({ FILL, fillKeys })
    const guide = svg.append("g").attr("id", "legend")
    const guideX = xScale(xDomain[1] * 0.9)
    const guideColorTextYOffset = 3.5
    const guideColorTextXOffset = 10
    const guideSizeYOffset = 70

    // Guide color colors
    guide.append("g")
        .selectAll("circle")
        .data(fillKeys)
        .join("circle")
        .attr("fill", i => fillPalette[i])
        .attr("cx", guideX)
        .attr("cy", i => guideY(i))
        .attr("r", 7)

    // Guide color text
    guide.append("g")
        .attr("font-size", fontSize*0.7)
        .selectAll("text")
        .data(fillKeys)
        .join("text")
        .attr("x", guideX + guideColorTextXOffset)
        .attr("y", i => guideY(i) + guideColorTextYOffset)
        // .attr("font-size", fontSize)
        .text(i => i)

    // guide size sizes
    guide.append("g")
        .selectAll("circle")
        .data(guideSizeBreaks)
        .join("circle")
        .attr("cx", guideX)
        .attr("cy", i => sizeGuideCircleY(i, 1))
        .attr("r", i => rScale(radius_from_area(i)))
        .attr("fill", "none")
        .attr("stroke", "black")

    // guide size lines 
    guide.append("g")
        .selectAll("line")
        .data(guideSizeBreaks)
        .join("line")
        .attr("x1", guideX )
        .attr("x2", guideX + 20)
        .attr("y1", i => sizeGuideCircleY(i, 2))
        .attr("y2", i => sizeGuideCircleY(i, 2))
        .attr("stroke", "black")
        .attr("stroke-width", "0.5px")

    // guide size text 
    guide.append("g")
        .attr("font-size", fontSize*0.7)
        .selectAll("text")
        .data(guideSizeBreaks)
        .join("text")
        .attr("x", guideX + 20)
        .attr("y", i => sizeGuideCircleY(i, 2))
        .text(i => i)


    function mousemove(e) {

        d3.select("circle.selected")
            .attr("class", null)
            .attr("stroke", "none")

        const targetID = e.target.id

        const labelID = LABEL[ID.indexOf(targetID)]

        var selectedCircle = d3.select(`circle#${targetID}`).node().getBoundingClientRect()
        var circleX = selectedCircle.x + selectedCircle.width + scrollX + 5
        var circleY = selectedCircle.y + scrollY - 25

        d3.select('div#scatter-tooltip')
            .style("visibility", "visible")
            .style("top", `${circleY}px`)
            .style("left", `${circleX}px`)
            .html(`<div>${labelID}</div>`)

        d3.select(`circle#${targetID}`)
            .attr("class", "selected")
            .attr("stroke", "red")
            .attr("stroke-width", strokeWidth)
            .raise()

    }
    return svg.node();
}
