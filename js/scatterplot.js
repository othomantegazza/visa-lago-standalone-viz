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
    marginTop = 25, // top margin, in pixels
    marginRight = 0, // right margin, in pixels
    marginBottom = 40, // bottom margin, in pixels
    marginLeft = 45, // left margin, in pixels
    inset = 0, // inset the default range, in pixels
    insetTop = inset, // inset the default y-range
    insetRight = inset, // inset the default x-range
    insetBottom = 0, // inset the default y-range
    insetLeft = inset, // inset the default x-range
    width = 640, // outer width, in pixels
    height = 400, // outer height, in pixels
    minWidth = 375,
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
    rRange = [0, 20],
    fillPalette, // an object with named colour
    fillOpacity = 0.9,
    curve = d3.curveLinear,  // method of interpolation between points
    fontSize = 14,
    fontTickReducer = 0.9,
    strokeWidth = 2.5, // stroke width for dots
    halo = "#fff", // color of label halo 
    haloWidth = 3, // padding around the labels,
    tooltipBackground = 'black',
    tooltipOffset = 10,
    highlightColor = '#b72dfc',
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
    const I = d3.range(data.length)
    let ORDER = orderIndex(X)

    // Compute default domains.
    if (xDomain === undefined) xDomain = [0, d3.max(X)]
    if (yDomain === undefined) yDomain = [0, d3.max(Y)]
    if (rDomain === undefined) rDomain = [0, d3.max(R)]

    // Construct scales and axes.
    const xScale = xType(xDomain, xRange)
    const yScale = yType(yDomain, yRange)
    const rScale = rType(rDomain, rRange)
    const xAxis = d3.axisBottom(xScale).ticks(width / 80, xFormat)
    const yAxis = d3.axisLeft(yScale).ticks(height / 50, yFormat)

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

    console.log({ dataForVoronoi, voronoiShow, render: voronoi.render(), renderCell: voronoi.renderCell(1) })


    console.log({
        x, y,
        xRange, yRange,
        X, Y, I,
        xDomain, yDomain,
        FILL, FITTED, fillPalette, ORDER,
        America: fillPalette["Europe"],
        voronoi,
        ID,
    })

    // generate tooltip
    const tooltip = d3.select("body")
        .append("div")
        .style("position", "absolute")
        .attr("id", "scatter-tooltip")

    // generate SVG
    const svg = d3.create("svg")
        .attr("width", width)
        .attr("height", height)
        .attr("viewBox", [0, 0, width, height])
        .attr("style", `max-width: 100%`)
        .attr("style", "cursor: crosshair;")       

    // axis x                  
    svg.append("g")
        .attr("transform", `translate(0,${height - marginBottom})`)
        .attr("class", "xaxis")
        .call(xAxis)
        .call(g => g.selectAll(".tick line").clone()
            .attr("y2", marginTop + marginBottom - height)
            .attr("stroke-opacity", 0.1))
        .call(g => g.selectAll(".tick text")
            .attr("font-size", fontSize))
        .call(g => g.append("text")
            .attr("x", width)
            .attr("y", marginBottom - 4)
            .text(xLabel));

    // axis y
    svg.append("g")
        .attr("transform", `translate(${marginLeft},0)`)
        .attr("class", "yaxis")
        .call(yAxis)
        .call(g => g.selectAll(".tick line").clone()
            .attr("x2", width - marginLeft - marginRight)
            .attr("stroke-opacity", 0.1))
        .call(g => g.selectAll(".tick text")
            .attr("font-size", fontSize))
        .call(g => g.append("text")
            .attr("x", -marginLeft)
            .attr("y", 10)
            .text(yLabel));

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
    //.attr("id", i => dateForID(X[i]))

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
        .on("mouseout", (e) => mouseout(e))

    function mousemove(e) {

        d3.select("circle.selected")
            .attr("class", null)
            .attr("stroke", "none")
            

        const targetID = e.target.id
        console.log({e, targetID})
        
        // d3.select(e.target).attr("fill", "yellow")

        d3.select('div#scatter-tooltip')
            .style("visibility", "visible")
            .style("top", `${e.pageY - tooltipOffset*2}px`)
            .style("left", `${e.pageX + tooltipOffset}px`)
            .html(`<div>${targetID}</div>`)

        d3.select(`circle#${targetID}`)
            .attr("class", "selected")
            .attr("stroke", "red")
            .attr("stroke-width", strokeWidth)
            .raise()
        }

    function mouseout(e) {

        const targetID = e.target.id

        console.log({e, targetID})

        d3.select('div#scatter-tooltip')
            .style("visibility", "hidden")
            
        d3.select(`circle#${targetID}`)
            .attr("stroke", "none")
        }
    //            .attr("class", "svg-tooltip")
    //            .attr("id", "tooltip-scatter")
    //            .style("visibility", "hidden")
    //            .style('top', event.pageY + 'px')
    //            .style(fromCorner, cornerDist)
    //            .style("visibility", "visible")
    //            .html(`${dateLabel}
    //                  <table id="table-scatterplot">
    //                  <th></th><th>µg/m<sup>3</sup></th><th>limits(%)</th>
    //                  ${poll_levels_string}
    //                  </table>`)


    //function pointermoved(event) { 
    //
    //      const input_millisec = xScale.invert(d3.pointer(event)[0])
    //      const millisec = (() => {
    //            if (input_millisec < start) {
    //                  return start
    //            } else if (input_millisec > end) {
    //                  return end
    //            } else {
    //                  return input_millisec
    //            }
    //            })()
    //      const floored_msec = millisec - (millisec % msec_per_day)
    //      const selector = dateForID(floored_msec)
    //      const dateLabel = dateForLabel(floored_msec)
    //      const selected_records = X.reduce(function(a, e, i) {
    //            if (e === floored_msec)
    //                a.push(i);
    //            return a;
    //        }, [])
    //      let poll_levels = Y
    //            .filter((lev, index) => selected_records.includes(index))
    //            .sort(function(a, b){return b-a})
    //      let poll_levels_colors = d3.map(poll_levels, i => `
    //            <tr>
    //            <td><span style="color: ${fillScale(i/euLimit)}">⬤</span></td>
    //            <td> ${d3.format(',.2r')(i)}</td>
    //            <td>${d3.format('.0%')(i/euLimit)}</td></tr>
    //            `)
    //      const poll_levels_string = poll_levels_colors.join('')
    //
    //      // distance from right coprner
    //      const tooltipX = event.pageX + tooltipOffsetPx
    //      const rightLimit = window.innerWidth - 200
    //
    //      // invert tooltip if too close to right corner
    //      let fromCorner
    //      let cornerDist
    //
    //      if (tooltipX > rightLimit) {
    //            fromCorner = 'right'
    //            cornerDist = window.innerWidth - (event.pageX - 15) + "px"
    //      } else {
    //            fromCorner = 'left'
    //            cornerDist =  event.pageX + 15 + "px"
    //      }
    //
    //      d3.selectAll("#tooltip-vline")
    //            .remove()
    //      
    //      d3.selectAll(".selectedCircle")
    //            .remove()
    //      
    //      d3.select("#tooltip-scatter")
    //            .remove()
    //
    //      svg.append("g")
    //            .attr("id", "tooltip-vline")
    //            .attr("stroke-width", 1)
    //            .attr("stroke", 'black')
    //            .append("line")
    //            .attr("x1", xScale(millisec))
    //            .attr("x2", xScale(millisec))
    //            .attr("y1", yScale(0))
    //            .attr("y2", yScale(d3.max(Y)))
    //      
    //
    //      d3.selectAll(`#${selector}`)
    //            .clone()
    //            .attr("class", "selectedCircle")
    //            .attr("stroke", highlightColor)
    //            .attr("r", r + r*rMultiplier)
    //
    //      // tooltip text
    //d3.select('#tooltip-heatmap-container').append("div")
    //            .attr("class", "svg-tooltip")
    //            .attr("id", "tooltip-scatter")
    //            .style("visibility", "hidden")
    //            .style('top', event.pageY + 'px')
    //            .style(fromCorner, cornerDist)
    //            .style("visibility", "visible")
    //            .html(`${dateLabel}
    //                  <table id="table-scatterplot">
    //                  <th></th><th>µg/m<sup>3</sup></th><th>limits(%)</th>
    //                  ${poll_levels_string}
    //                  </table>`)
    //
    //      
    //}
    //
    //function pointerleft() {
    //
    //      d3.selectAll(".selectedCircle")
    //            .remove()
    //
    //      d3.select("#tooltip-vline")
    //            .attr("visibility", "hidden")
    //
    //      d3.select("#tooltip-scatter")
    //            .remove()
    //}
    //
    //function dateForID(msec) {
    //      const formatted = dayjs(msec)
    //      return(`d${formatted.$y}_${formatted.$M}_${formatted.$D}`)
    //}
    //
    //function dateForLabel(msec) {
    //      const formatted = dayjs(msec)
    //      return(`${formatted.$D}-${formatted.$M + 1}-${formatted.$y}`)
    //}
    //
    return svg.node();
}
