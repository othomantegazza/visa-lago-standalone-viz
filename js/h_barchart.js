function h_barchart(data, {
  label = d => d.label,
  value = d => d.value,
  continent = d => d.continent,
  total = d => d.total,
  width = 1000,
  barHeight = 25,
  barSpacing = 10,
  marginTop = 20,
  marginRight = 100,
  marginBottom = 50,
  marginLeft = 300,
  fontSize = 18,
  fontSizeValue = 16,
  symbol = "%"
} = {}) {

  const cb_palette = {
    Africa: "#228833",
    Americas: "#CCBB44",
    Asia: "#4477AA",
    Europe: "#EE6677",
    Oceania: "#AA3377"
  };

  const labels = data.map(label);
  const values = data.map(value);
  const totals = data.map(total);
  const colors = data.map(d => cb_palette[continent(d)]);

  const height = (barHeight + barSpacing) * data.length + marginTop + marginBottom;

  const svg = d3.create("svg")
    .attr("width", width)
    .attr("height", height)
    .attr("viewBox", [0, 0, width, height])
    .attr("class", "barchart")
    .style("max-width", "100%")
    .style("cursor", "crosshair")
    .style("margin", "auto")
    .style("display", "block");

  const xScale = d3.scaleLinear([0, d3.max(values)], [0, width - marginLeft - marginRight]);

  const yScale = d3.scaleBand()
    .domain(labels)
    .range([marginTop, height - marginBottom])
    .paddingInner(0.1);

  const yAxis = d3.axisLeft(yScale).tickSize(0);

  svg.append("g")
    .attr("transform", `translate(${marginLeft},0)`)
    .attr("class", "yaxis")
    .call(yAxis)
    .call(g => g.selectAll(".tick text")
      .attr("font-size", fontSize)
      .attr("font-family", "Manrope"))
    .call(g => g.select(".domain").remove());
    

  setTimeout(() => {
    svg.selectAll(".tick text")
      .each(function() {
        d3.select(this).call(wrap, marginLeft);
      });
  }, 0);

  // barre arrotondate con colori personalizzati
  svg.append("g")
    .attr("transform", `translate(${marginLeft + 20},10)`)
    .selectAll("rect")
    .data(data)
    .join("rect")
    .attr("x", 0)
    .attr("y", d => yScale(label(d)) + barSpacing / 2)
    .attr("height", barHeight)
    .attr("width", d => xScale(value(d)))
    .attr("fill", (d,i) => colors[i])
    .attr("rx", 13);
  
  svg.append("g")
    .attr("transform", `translate(${marginLeft + 40}, 37)`)
    .selectAll("text")
    .data(data)
    .join("text")
    .attr("x", 0)
    .attr("y", d => yScale(label(d)) - 5)
    .attr("font-size", fontSizeValue)
    .attr("fill", "white")
    .attr("font-family", "Manrope")
    .attr("font-weight", 500)
    .text(d => "Total applications: " + total(d));

  // valori sopra barre
  svg.append("g")
    .attr("transform", `translate(${marginLeft + 20},14)`)
    .selectAll("text")
    .data(data)
    .join("text")
    .attr("x", d => xScale(value(d)) + 6)
    .attr("y", d => yScale(label(d)) + barSpacing / 2 + barHeight / 2)
    .attr("font-size", fontSizeValue)
    .attr("font-family", "Manrope")
    .attr("font-weight", 700)
    .attr("dominant-baseline", "middle")
    .text(d => value(d) + symbol);

  return svg.node();
}

function wrap(text, wrapWidth) {
  text.each(function () {
    var text = d3.select(this),
      words = text.text().split(/\s+/).reverse(),
      word,
      line = [],
      y = text.attr("y"),
      dy = 1,
      tspan = text.text(null)
        .append("tspan")
        .attr("x", 0)
        .attr("y", y)
        .attr("dy", `${dy}em`);
    while (word = words.pop()) {
      line.push(word);
      tspan.text(line.join(" "));
      if (tspan.node().getComputedTextLength() > wrapWidth) {
        line.pop();
        tspan.text(line.join(" "));
        line = [word];
        tspan = text.append("tspan")
          .attr("x", 0)
          .attr("y", y)
          .attr("dy", dy + "em")
          .text(word);
      }
    }
  });
  return 0;
}
