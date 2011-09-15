package org.netbeans.api.javafx.source;

import javafx.scene.CustomNode;
import javafx.scene.Group;
import javafx.scene.Node;
import javafx.stage.Stage;
import javafx.scene.Scene;

var seq = [ 1,2,3,4,5];

var subseq = seq[ selector | selector < 3];

class MyNode extends CustomNode {
        function doit() {}
	public override function create(): Node {
		return Group {
			content: []
		};
	}
}

Stage {
	title : bind daName;
	scene: Scene {
		width: 200
		height: 200
		content: [ MyNode{}]
	}
}

var daName = "MyApp";