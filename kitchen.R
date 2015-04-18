# Draw kitchen floorplan with ggplot2
# bottom left corner (starting point) at (1,1).

library("ggplot2")
cut   <- 44    # width in inches of 45-degree cut into kitchen (wall 2).
thk   <- 2     # wall thickness, for drawing purposes.
cwd   <- 25.25 # counter depth all the way to wall, including raised part.
frw   <- 3     # fridge clearance from the 2 walls

# walls

# vertical shapes for wall 1, facing front door:
# two cutouts for total width of pantry door (35 in) 
# and kitchen's front door (37 in) where total width
# is measured including the width of the millwork.
x1  <- rep(c(1,1+thk),each=2)
y1  <- c(c(1,1+8.5),c(1+8.5,1))
x2  <- x1
y2  <- y1 + 1 + 8.5 + 35
x3  <- x1
y3  <- y1 + 1 + 8.5 + 35 + 8.5 + 37

# triangular shape connecting wall 1 to wall 2
x4   <- c(1,1,1+thk)
y4   <- c(y3[2],y3[2]+thk,y3[2])

# wall 2
x5   <- c(1,1+sqrt(cut^2/2),1+sqrt(cut^2/2)+sqrt(2*thk^2),1+thk)
y5   <- c(y4[2],y4[2]+sqrt(cut^2/2),y4[1]+sqrt(cut^2/2),y4[1])

# triangular shape connecting wall 2 to wall 3
x6   <- c(x5[2],x5[3],x5[3])
y6   <- c(y5[2],y5[2],y5[3])

# wall 3, facing dining area
x7   <- rep(c(x6[2],x6[2]+31.5),each=2)
y7   <- c(c(y6[3],y6[2]),c(y6[2],y6[3]))
x8   <- x7 + 31.5+43
y8   <- y7

# wall 4, with kitchen window
x9   <- rep(c(x8[4],x8[4]+thk),each=2)
y9   <- c(y8[2],y8[2]-(5*12+65+thk),y8[2]-(5*12+65+thk),y8[2])

# wall 5, behind the stove
x10  <- c(rep(x9[1] - (25+66+12),2),rep(x9[4],2))
y10  <- c(y9[2],y9[2]-thk,y9[2]-thk,y9[2])

# bottom layer: fridge, stove, countertops.

# fridge
x11  <- rep(c(x9[4]-thk-frw,x9[4]-thk-frw-30),each=2)
y11  <- c(y9[4]-thk-frw,y9[4]-thk-frw-30,y9[4]-thk-frw-30,y9[4]-thk-frw)

# stove
x12  <- rep(c(x9[4]-thk-4*12,x9[4]-thk-4*12-30),each=2)
y12  <- c(y10[1],y10[1]+30,y10[1]+30,y10[1])

# pantry
x13  <- rep(c(x12[4]-24,x12[4]),each=2)
y13  <- c(y10[1],y10[1]+24,y10[1]+24,y10[1])

# The counter with kitchen sink (along walls 4 and 5 between fridge and stove)
# is L-shaped. Leave .25in clearance between counter end and fridge.
x14  <- rep(c(x9[1]-cwd,x9[1]),each=2)
y14  <- c(y10[1],y11[2]-.25,y11[2]-.25,y10[1])
x15  <- rep(c(x14[1]-22,x14[1]),each=2)
y15  <- c(y14[1],y14[1]+cwd,y14[1]+cwd,y14[1])   

# The odd-shaped counter is made up of an equilateral trapeze (1) and
# a right trapezoid (2). The angles of (1) are (65, 65, 115, 115) 
# and the angles of (2) are (65, 90, 90, 115).

# length of the side that's toward the door: 2-step Pythagoras.
c1 <- cwd * tan(25*pi/180) # c as in short cathetus
h1 <- sqrt(c1^2+cwd^2)     # h as in hypothenuse
# angle between h1 and third segment of first wall
a0 <- 90 + 45 - 65
# ratio of the two catheti
r0 <- tan(a0*pi/180)
c0 <- sqrt(h1^2/(1+r0^2))

# These are the (x,y) coordinates of the first obtuse angle of trapeze (1),
# the you'd brush against if you came into the kitchen through its front 
# door and for some reason you decided to veer left. Where Joe's bowls are.
x0 <- c(x3[3]+c0*r0, NA)
y0 <- c(y3[3]-c0,NA)
# The shorter end of the trapeze is parallel to the 45-degree wall, so
# for the (x,y) coordinates of the second obtuse angle you just need
# the measured length of this shorter end, which is 22.5 inches.
x0[2] <- x0[1]+sqrt(22.5^2/2)
y0[2] <- y0[1]+sqrt(22.5^2/2)
# Now the whole shebang
x16 <- c(x5[4],x0,x5[3])
y16 <- c(y5[4],y0,y5[3])

# Now for the right trapezoid (2):
# The measured sharp angle of 65 can't be right if the walls
# are perfectly straight as plotted here. If wall 2 is at
# a 45-degree angle, then the wide sides of it each are
# 45 + 90 degrees, so the sharp angle of the trapezoid
# (2) should be 45 + 90 - 65 = 70, which would mean that
# the wide angle is 180 - 70 = 110 degrees: not the same 
# 65 / 115 that comes out in actual measurements. But
# the 65 / 115 symmetry between the left side of (2)
# and the right side of (1) is what guarantees that the 
# two chunks of counter have the same depth cwd. If you 
# instead trust the calculated 70 / 110 angles for (2)
# you get a counter (2) that is shallower than cwd: the
# difference y17[3]-y17[2] works out to 24.73 inches.
# Evidently my walls aren't straight. 
x17 <- c(x16[3],rep(x16[3]+20.5,2),x16[4])
y17 <- c(y16[3],y16[3],y16[4],y16[4])

# Now the footprint of the drywall fill work overhead:
# GOT HERE!

# filler color for walls vs. counters vs. appliances
values <- data.frame(
   id = c(1:17),
   value = factor(c(rep(3,10),4,5,6,rep(7,4)),
                  labels=c('wall','fridge','stove','pantry','counter'))
)

pos <- data.frame(id=c(rep(c(1:3),each = 4),
                       rep(4,3),rep(5,4),
                       rep(6,3),rep(c(7:17),each=4)),
                  x=c(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17),
                  y=c(y1,y2,y3,y4,y5,y6,y7,y8,y9,y10,y11,y12,y13,y14,y15,y16,y17))

pos <- merge(pos,values)

pic <- ggplot() + geom_polygon(data=pos, mapping=aes(x=x, y=y, group=id, fill=value)) + 
   annotate(geom='text', x=(x11[1]+x11[3])/2, y=(y11[1]+y11[3])/2, label="fridge") + 
   annotate(geom='text', x=(x12[1]+x12[3])/2, y=(y12[1]+y12[3])/2, label="stove") +
   annotate(geom='text', x=(x13[1]+x13[3])/2, y=(y13[1]+y13[3])/2, label="pantry") +
   coord_fixed()

