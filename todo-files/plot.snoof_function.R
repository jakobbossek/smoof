# @export
plot.smoof_function = function(x, ...) {
    checkPlsmoofunParams(x)

    mapping = list("1Dnumeric" = plot1DNumeric, "2Dnumeric" = plot2DNumeric, "2DMixed" = plot2DMixed)
    plsmoofun = getInternalPlsmoofunction(x, mapping = mapping)

    plsmoofun(x, ...)
}

plot1DNumeric = function(x, ...) {
    #FIXME: this is copy and paste crap!
    #FIXME: There should be one internal plot function, which generates the data?
    # extract data
    par.set = getParamSet(x)
    par.name = getParamIds(par.set)

    # get lower and upper bounds
    lower = getBounds(bound = getLower(par.set), default = -10L)
    upper = getBounds(bound = getUpper(par.set), default = 10L)

    #FIXME: by = 0.01 is evil!
    data = generateDataframeForGGPlot(fn = x, sequences = list(seq(lower, upper, by = 0.01)), par.set = par.set)

    plot(x = data[[par.name]], y = data[["y"]], type = "l",
        xlab = par.name, ylab = "y")
}

plot2DNumeric = function(x, ...) {
    par.set = getParamSet(x)
    par.names = getParamIds(par.set)

    lower = getBounds(bound = getLower(par.set), default = -10L)
    upper = getBounds(bound = getUpper(par.set), default = 10L)

    sequence.x1 = seq(lower[1], upper[1], length.out = 150)
    sequence.x2 = seq(lower[2], upper[2], length.out = 150)
    sequences = list(sequence.x1, sequence.x2)
    data = generateDataframeForGGPlot(x, sequences, par.set)

    # ugly! make matrix out of z values. Required for 'image'
    z = data[["y"]]
    dim(z) <- c(150, 150)

    image(x = sequence.x1, y = sequence.x2, z = z,
        xlab = par.names[1], ylab = par.names[2],
        col = terrain.colors(100)
    )
}

plot2DMixed = function(x, ...) {}