package org.netbeans.api.visage.source;

import visage.scene.CustomNode;
import visage.scene.Group;
import visage.scene.Node;
import visage.stage.Stage;
import visage.scene.Scene;

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