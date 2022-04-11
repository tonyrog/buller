# buller
anarchistic bulletin board

# Starting

    erl -s buller
	
# Run

	cd buller/priv
	firefox buller.html
	
or kiosk mode, FULL SCREEN! use ctrl-q to quit

	firefox --kiosk --private-window buller.html

# commands

	wget -q -O - --method=POST --body-data="x=10&y=10&width=400&height=400&color=green" http://localhost:1235/v1/draw_rect

	wget -q -O - --method=POST --body-data="x=100&y=100&font-size=70&color=black&text=Hello" http://localhost:1235/v1/draw_text
	
	wget -q -O - --method=POST --body-data="x=200&y=200&font-size=70&color=red&text=World" http://localhost:1235/v1/draw_text

	wget -q -O - --method=POST --body-data="x=200&y=200&src=https://www.w3schools.com/jsref/img_the_scream.jpg" http://localhost:1235/v1/draw_image

	wget -q -O - --method=POST --body-data="x=200&y=200&src=https://www.w3schools.com/jsref/mov_bbb.mp4" http://localhost:1235/v1/draw_video
	
	wget -q -O - --method=POST --body-data="x=10&y=10&color=#01020304" http://localhost:1235/v1/draw_pixel
	
	wget -q -O - "http://localhost:1235/pixel?x=10&y10"


# get size, width and height of buller canvas

    wget -q -O - http://localhost:1235/height
	
    wget -q -O - http://localhost:1235/width
