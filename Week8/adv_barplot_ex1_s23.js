// These files are typically called app.js.
// For teaching purposes I will align the html and js file names for matching.

// Normally they would be called index.html and likely app.js. 
// Why index.html? Because that's the default name used for a home page. 

// Today I will hard code some data from a quick summary done in R with the WIID data. 

// The data are entered in JSON format, as arrays of objects. 
// Think of JSON data as like row-wise lists. 

const wiid_data = [
    {region: 'Africa', meangini:46.1},
    {region: 'Americas', meangini:46.0},
    {region: 'Asia', meangini:36.5},
    {region: 'Europe', meangini:31.7},
    {region: 'Oceania', meangini:37.0},
  ]
  
  // We add new scale functions for letting the svg define a scale according to our data.
  // The domain tells us how many 
  // scaleBand is for categorical or ordinal values, equally spaced. 
  // rangeRound tells how much space in the svg is available. 
  
  const xScale = d3.scaleBand()
    .domain(wiid_data.map(dataPoint => dataPoint.region)) // Total items/spaces on x-axis. 
    .range([0, 750]) // Info on min/max in pixels. 
    .padding(0.2); // Inner spacing.  
  
  // Create a y scale function that we will use later. 
  // The 'domain' lets us define the minimum and maximum for the scale. 
  // The 'range' method tells the actual space in pixels. Start with the MAX, not the MIN.
  // ...Why? Because of the top left orientation of the svg element. 

  const yScale = d3.scaleLinear().domain([0,55]).range([600,0]);
  
  const container = d3.select('svg') 
    .classed('container', true); // Create a new container class for our svg. 

  const bars = container
    .selectAll('.bar') // selecting future containers of class bars
    .data(wiid_data)
    .enter() // checks to see if any of these objects are missing
    .append('rect') // 'rect' is one of a few valid svg elements. 
    .classed('bar',true)
    .attr('width', xScale.bandwidth()) // Call the xScale function we made. 
    .attr('height', data => 600 - yScale(data.meangini)) // Same as above, but for y. Reverse.
    .attr('x', data => xScale(data.region)) // Use xScale function.
    .attr('y', data => yScale(data.meangini));

// Adding a hover color change is actually pretty simple. 
// This actually throws a little jQuery into the mix. 

    svg.selectAll(".bar")
    .on("mouseover", function() {
      d3.select(this).style("fill", "orange");
    })
    .on("mouseout", function() {
      d3.select(this).style("fill", "red");
    });
