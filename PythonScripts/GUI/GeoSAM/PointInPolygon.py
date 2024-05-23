# First of all, notice that each iteration considers two adjacent points and the target point. 
# Then the if statement evaluates two conditions:
#
# 1.) Y-value of our target point is within the range [verty[j], verty[i]).
# 2.) X-value of our target point is below the linear line connecting the point j and i.
# If you're having problems to see this second condition, just write down the linear equation of the line,
# reorganize the expression a little bit and place testy as the free variable.
#
# Every time the above two conditions are met, we toggle the flag c. So we return true if 
# above conditions are met odd number of times and false otherwise.
#
# http://alienryderflex.com/polygon/

def PointInPolygon(polyX, polyY, x, y, nodes):
    """
    @param float polyX Array of horizontal, coordinates of corners
    @param float polyY Array of vertical    coordinates of corners
    @param float x horizontal coordinate of point we wish to determine if inside polygram
    @param float y vertical   coordinate of point we wish to determine if inside polygram
    @param int   nodes the number of corners, edges, that define the polygon
    @returns true if point is inside polygram or if on vert or horiz edge,
             false if outside
             if point is on rise of falling edge then it may return true or false
    """

    inPolygon = False
    j = nodes - 1

    for i in range(nodes):
        ind1 = polyY[i] < y and polyY[j] >= y
        ind2 = polyY[j] < y and polyY[i] >= y
        # as the diff approaches 0, 1/diff approaches infinity which is > x
        # if diff is negative then ind3 is False
        # is diff is positive then ind3 is True
        if (polyY[j] - polyY[i]) == 0.0:
            ind3 = False
        else:
            ind3 = polyX[i] + (y - polyY[i]) / (polyY[j] - polyY[i]) * (polyX[j] - polyX[i]) < x
        if ((ind1 or ind2) and (ind3)): inPolygon = not inPolygon
        j = i

    return inPolygon
