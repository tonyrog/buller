# buller
anarchistic bulletin board

# Starting

    erl -s buller
	
# Run

	cd buller/priv
	firefox buller.html

# rect/text commands

	wget -q -O - --method=POST --body-data="x=10&y=10&width=400&height=400&color=green" http://localhost:1235/rect

	wget -q -O - --method=POST --body-data="x=100&y=100&font-size=70&color=black&text=Hello" http://localhost:1235/text
	
	wget -q -O - --method=POST --body-data="x=200&y=200&font-size=70&color=red&text=World" http://localhost:1235/text

# get size, width and height of buller canvas

    wget -q -O - http://localhost:1235/height
	
    wget -q -O - http://localhost:1235/width
