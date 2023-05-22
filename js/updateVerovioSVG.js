function updateSvgDisplay(id, options) {
	if (!vrvToolkit) {
		VEROVIO_QUEUE.push( { id:id, options:options });
		console.log("verovio not found, queuing notation rendering for", id);
		return;
	}
	var targetid = id + "-svg";
	var selement = document.querySelector("#" + targetid);
	if (!selement) {
		console.log("Error: could not find svg target #" + targetid);
		return;
	}
	var humid = id + "-humdrum";
	var helement = document.querySelector("#" + humid);
	if (!helement) {
		console.log("Error: could not find Humdrum source #" + humid);
		return;
	}
	var content = helement.textContent.replace(/^\s+/, "");
	if (content=="") return;
	var cleaned_options = cleanOptions(content, options);

	var svg = vrvToolkit.renderData(content, cleaned_options);
	selement.innerHTML = svg;
	// automatically set the height of the Humdrum element to be the
   // same size as the height of the SVG (barring limitations on the
	// min and max height given in CSS.
	helement.style.height = (selement.offsetHeight-40) + "px";
}

function cleanOptions(content, options) {
	var lines = content.match(/[^\r\n]+/g);
	var output = options;
	var setlist = [""];
	var optionsets = {};
	optionsets[""] = {};
	var i;
	for (i=0; i<lines.length; i++) {
		var matches = lines[i].match(/^!!!verovio([^\s]*):\s*(.*)\s*$/);
		if (!matches) {
			continue;
		}
		if (matches[1] == "-parameter-group") {
			setlist.push(matches[2]);
			continue;
		}
		var mm = matches[2].match(/^\s*([^\s]+)\s+(.*)\s*$/);
		if (!mm) {
			continue;
		}
		var m = matches[1].match(/^-([^\s]+)\s*$/);
		var set = "";
		if (m) {
			set = m[1];
		}
		if (typeof optionsets[set] === 'undefined') {
			optionsets[set] = {};
		}
		optionsets[set][mm[1]] = mm[2];
	}

	for (i=0; i<setlist.length; i++) {
		if (!optionsets[setlist[i]]) {
			continue;
		}
		var keys = Object.keys(optionsets[setlist[i]]);
		var j;
		var key;
		for (j=0; j<keys.length; j++) {
			if (typeof output[keys[j]] !== 'undefined') {
				delete output[keys[j]];
			}
		}
	}

	return output;
}