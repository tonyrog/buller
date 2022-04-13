#!/bin/sh -x
BULLER_DIR=$HOME/erlang/buller
BULLER=$BULLER_DIR/priv/buller

$BULLER draw_image x=100 y=100 src=images/ros.png
$BULLER text id=t1 x=100 y=100 color=blue font-size=50 text=Hello
$BULLER path id=g1 d="M350 0 L75 300 L325 300 Z" color=green
$BULLER path id=g2 d="M150 0 L75 200 L225 200 Z" color=red

