var theTrace = {name: "top-level",
            formals: [],
            actuals: [],
            result: "'no-result",
            children: [{name: "remove-min",
            formals: ["'(make-heap (list 8 4 3 9 1 6 12 14))"],
            actuals: ["'#(struct:bhnode\n   1\n   #(struct:bhnode\n     6\n     #(struct:bhnode\n       9\n       #(struct:bhnode 14 () ())\n       ())\n     #(struct:bhnode 8 () ()))\n   #(struct:bhnode\n     3\n     #(struct:bhnode 12 () ())\n     #(struct:bhnode 4 () ())))"],
            result: "'#(struct:bhnode\n   3\n   #(struct:bhnode\n     4\n     #(struct:bhnode 12 () ())\n     #(struct:bhnode 14 () ()))\n   #(struct:bhnode\n     6\n     #(struct:bhnode 8 () ())\n     #(struct:bhnode 9 () ())))",
            children: [{name: "bhnode-left",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   1\n   #(struct:bhnode\n     6\n     #(struct:bhnode\n       9\n       #(struct:bhnode 14 () ())\n       ())\n     #(struct:bhnode 8 () ()))\n   #(struct:bhnode\n     3\n     #(struct:bhnode 12 () ())\n     #(struct:bhnode 4 () ())))"],
            result: "'#(struct:bhnode\n   6\n   #(struct:bhnode\n     9\n     #(struct:bhnode 14 () ())\n     ())\n   #(struct:bhnode 8 () ()))",
            children: []},{name: "bhnode?",
            formals: ["'l"],
            actuals: ["'#(struct:bhnode\n   6\n   #(struct:bhnode\n     9\n     #(struct:bhnode 14 () ())\n     ())\n   #(struct:bhnode 8 () ()))"],
            result: "#t",
            children: []},{name: "bhnode-left",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   6\n   #(struct:bhnode\n     9\n     #(struct:bhnode 14 () ())\n     ())\n   #(struct:bhnode 8 () ()))"],
            result: "'#(struct:bhnode\n   9\n   #(struct:bhnode 14 () ())\n   ())",
            children: []},{name: "bhnode?",
            formals: ["'l"],
            actuals: ["'#(struct:bhnode\n   9\n   #(struct:bhnode 14 () ())\n   ())"],
            result: "#t",
            children: []},{name: "bhnode-left",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   9\n   #(struct:bhnode 14 () ())\n   ())"],
            result: "'#(struct:bhnode 14 () ())",
            children: []},{name: "bhnode?",
            formals: ["'l"],
            actuals: ["'#(struct:bhnode 14 () ())"],
            result: "#t",
            children: []},{name: "bhnode-left",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode 14 () ())"],
            result: "'()",
            children: []},{name: "bhnode?",
            formals: ["'l"],
            actuals: ["'()"],
            result: "#f",
            children: []},{name: "bhnode-value",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode 14 () ())"],
            result: "14",
            children: []},{name: "bhnode-left",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   1\n   #(struct:bhnode\n     6\n     #(struct:bhnode\n       9\n       #(struct:bhnode 14 () ())\n       ())\n     #(struct:bhnode 8 () ()))\n   #(struct:bhnode\n     3\n     #(struct:bhnode 12 () ())\n     #(struct:bhnode 4 () ())))"],
            result: "'#(struct:bhnode\n   6\n   #(struct:bhnode\n     9\n     #(struct:bhnode 14 () ())\n     ())\n   #(struct:bhnode 8 () ()))",
            children: []},{name: "bhnode?",
            formals: ["'l"],
            actuals: ["'#(struct:bhnode\n   6\n   #(struct:bhnode\n     9\n     #(struct:bhnode 14 () ())\n     ())\n   #(struct:bhnode 8 () ()))"],
            result: "#t",
            children: []},{name: "make-bhnode",
            formals: ["'(bhnode-value h)","'(bhnode-right h)","'(remove-left-most l)"],
            actuals: ["1","'#(struct:bhnode\n   3\n   #(struct:bhnode 12 () ())\n   #(struct:bhnode 4 () ()))","'#(struct:bhnode\n   6\n   #(struct:bhnode 8 () ())\n   #(struct:bhnode 9 () ()))"],
            result: "'#(struct:bhnode\n   1\n   #(struct:bhnode\n     3\n     #(struct:bhnode 12 () ())\n     #(struct:bhnode 4 () ()))\n   #(struct:bhnode\n     6\n     #(struct:bhnode 8 () ())\n     #(struct:bhnode 9 () ())))",
            children: []},{name: "bhnode-value",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   1\n   #(struct:bhnode\n     6\n     #(struct:bhnode\n       9\n       #(struct:bhnode 14 () ())\n       ())\n     #(struct:bhnode 8 () ()))\n   #(struct:bhnode\n     3\n     #(struct:bhnode 12 () ())\n     #(struct:bhnode 4 () ())))"],
            result: "1",
            children: []},{name: "bhnode-right",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   1\n   #(struct:bhnode\n     6\n     #(struct:bhnode\n       9\n       #(struct:bhnode 14 () ())\n       ())\n     #(struct:bhnode 8 () ()))\n   #(struct:bhnode\n     3\n     #(struct:bhnode 12 () ())\n     #(struct:bhnode 4 () ())))"],
            result: "'#(struct:bhnode\n   3\n   #(struct:bhnode 12 () ())\n   #(struct:bhnode 4 () ()))",
            children: []},{name: "bhnode-left",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   6\n   #(struct:bhnode\n     9\n     #(struct:bhnode 14 () ())\n     ())\n   #(struct:bhnode 8 () ()))"],
            result: "'#(struct:bhnode\n   9\n   #(struct:bhnode 14 () ())\n   ())",
            children: []},{name: "bhnode?",
            formals: ["'l"],
            actuals: ["'#(struct:bhnode\n   9\n   #(struct:bhnode 14 () ())\n   ())"],
            result: "#t",
            children: []},{name: "make-bhnode",
            formals: ["'(bhnode-value h)","'(bhnode-right h)","'(remove-left-most l)"],
            actuals: ["6","'#(struct:bhnode 8 () ())","'#(struct:bhnode 9 () ())"],
            result: "'#(struct:bhnode\n   6\n   #(struct:bhnode 8 () ())\n   #(struct:bhnode 9 () ()))",
            children: []},{name: "bhnode-value",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   6\n   #(struct:bhnode\n     9\n     #(struct:bhnode 14 () ())\n     ())\n   #(struct:bhnode 8 () ()))"],
            result: "6",
            children: []},{name: "bhnode-right",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   6\n   #(struct:bhnode\n     9\n     #(struct:bhnode 14 () ())\n     ())\n   #(struct:bhnode 8 () ()))"],
            result: "'#(struct:bhnode 8 () ())",
            children: []},{name: "bhnode-left",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   9\n   #(struct:bhnode 14 () ())\n   ())"],
            result: "'#(struct:bhnode 14 () ())",
            children: []},{name: "bhnode?",
            formals: ["'l"],
            actuals: ["'#(struct:bhnode 14 () ())"],
            result: "#t",
            children: []},{name: "make-bhnode",
            formals: ["'(bhnode-value h)","'(bhnode-right h)","'(remove-left-most l)"],
            actuals: ["9","'()","'()"],
            result: "'#(struct:bhnode 9 () ())",
            children: []},{name: "bhnode-value",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   9\n   #(struct:bhnode 14 () ())\n   ())"],
            result: "9",
            children: []},{name: "bhnode-right",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   9\n   #(struct:bhnode 14 () ())\n   ())"],
            result: "'()",
            children: []},{name: "bhnode-left",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode 14 () ())"],
            result: "'()",
            children: []},{name: "bhnode?",
            formals: ["'l"],
            actuals: ["'()"],
            result: "#f",
            children: []},{name: "bhnode?",
            formals: ["'new-h"],
            actuals: ["'#(struct:bhnode\n   1\n   #(struct:bhnode\n     3\n     #(struct:bhnode 12 () ())\n     #(struct:bhnode 4 () ()))\n   #(struct:bhnode\n     6\n     #(struct:bhnode 8 () ())\n     #(struct:bhnode 9 () ())))"],
            result: "#t",
            children: []},{name: "bhnode-left",
            formals: ["'new-h"],
            actuals: ["'#(struct:bhnode\n   1\n   #(struct:bhnode\n     3\n     #(struct:bhnode 12 () ())\n     #(struct:bhnode 4 () ()))\n   #(struct:bhnode\n     6\n     #(struct:bhnode 8 () ())\n     #(struct:bhnode 9 () ())))"],
            result: "'#(struct:bhnode\n   3\n   #(struct:bhnode 12 () ())\n   #(struct:bhnode 4 () ()))",
            children: []},{name: "bhnode-right",
            formals: ["'new-h"],
            actuals: ["'#(struct:bhnode\n   1\n   #(struct:bhnode\n     3\n     #(struct:bhnode 12 () ())\n     #(struct:bhnode 4 () ()))\n   #(struct:bhnode\n     6\n     #(struct:bhnode 8 () ())\n     #(struct:bhnode 9 () ())))"],
            result: "'#(struct:bhnode\n   6\n   #(struct:bhnode 8 () ())\n   #(struct:bhnode 9 () ()))",
            children: []},{name: "bhnode?",
            formals: ["'y"],
            actuals: ["'#(struct:bhnode\n   3\n   #(struct:bhnode 12 () ())\n   #(struct:bhnode 4 () ()))"],
            result: "#t",
            children: []},{name: "get-min",
            formals: ["'y"],
            actuals: ["'#(struct:bhnode\n   3\n   #(struct:bhnode 12 () ())\n   #(struct:bhnode 4 () ()))"],
            result: "3",
            children: []},{name: "bhnode?",
            formals: ["'r"],
            actuals: ["'#(struct:bhnode\n   6\n   #(struct:bhnode 8 () ())\n   #(struct:bhnode 9 () ()))"],
            result: "#t",
            children: []},{name: "get-min",
            formals: ["'l"],
            actuals: ["'#(struct:bhnode\n   3\n   #(struct:bhnode 12 () ())\n   #(struct:bhnode 4 () ()))"],
            result: "3",
            children: []},{name: "get-min",
            formals: ["'r"],
            actuals: ["'#(struct:bhnode\n   6\n   #(struct:bhnode 8 () ())\n   #(struct:bhnode 9 () ()))"],
            result: "6",
            children: []},{name: "make-bhnode",
            formals: ["'(get-min l)","'(insert-merge\n  x\n  (bhnode-left l)\n  (bhnode-right l))","'r"],
            actuals: ["3","'#(struct:bhnode\n   4\n   #(struct:bhnode 12 () ())\n   #(struct:bhnode 14 () ()))","'#(struct:bhnode\n   6\n   #(struct:bhnode 8 () ())\n   #(struct:bhnode 9 () ()))"],
            result: "'#(struct:bhnode\n   3\n   #(struct:bhnode\n     4\n     #(struct:bhnode 12 () ())\n     #(struct:bhnode 14 () ()))\n   #(struct:bhnode\n     6\n     #(struct:bhnode 8 () ())\n     #(struct:bhnode 9 () ())))",
            children: []},{name: "get-min",
            formals: ["'l"],
            actuals: ["'#(struct:bhnode\n   3\n   #(struct:bhnode 12 () ())\n   #(struct:bhnode 4 () ()))"],
            result: "3",
            children: []},{name: "bhnode-left",
            formals: ["'l"],
            actuals: ["'#(struct:bhnode\n   3\n   #(struct:bhnode 12 () ())\n   #(struct:bhnode 4 () ()))"],
            result: "'#(struct:bhnode 12 () ())",
            children: []},{name: "bhnode-right",
            formals: ["'l"],
            actuals: ["'#(struct:bhnode\n   3\n   #(struct:bhnode 12 () ())\n   #(struct:bhnode 4 () ()))"],
            result: "'#(struct:bhnode 4 () ())",
            children: []},{name: "bhnode?",
            formals: ["'y"],
            actuals: ["'#(struct:bhnode 12 () ())"],
            result: "#t",
            children: []},{name: "get-min",
            formals: ["'y"],
            actuals: ["'#(struct:bhnode 12 () ())"],
            result: "12",
            children: []},{name: "bhnode?",
            formals: ["'r"],
            actuals: ["'#(struct:bhnode 4 () ())"],
            result: "#t",
            children: []},{name: "get-min",
            formals: ["'l"],
            actuals: ["'#(struct:bhnode 12 () ())"],
            result: "12",
            children: []},{name: "get-min",
            formals: ["'r"],
            actuals: ["'#(struct:bhnode 4 () ())"],
            result: "4",
            children: []},{name: "bhnode?",
            formals: ["'l"],
            actuals: ["'#(struct:bhnode 12 () ())"],
            result: "#t",
            children: []},{name: "get-min",
            formals: ["'r"],
            actuals: ["'#(struct:bhnode 4 () ())"],
            result: "4",
            children: []},{name: "get-min",
            formals: ["'l"],
            actuals: ["'#(struct:bhnode 12 () ())"],
            result: "12",
            children: []},{name: "make-bhnode",
            formals: ["'(get-min r)","'l","'(insert-merge\n  x\n  (bhnode-left r)\n  (bhnode-right r))"],
            actuals: ["4","'#(struct:bhnode 12 () ())","'#(struct:bhnode 14 () ())"],
            result: "'#(struct:bhnode\n   4\n   #(struct:bhnode 12 () ())\n   #(struct:bhnode 14 () ()))",
            children: []},{name: "get-min",
            formals: ["'r"],
            actuals: ["'#(struct:bhnode 4 () ())"],
            result: "4",
            children: []},{name: "bhnode-left",
            formals: ["'r"],
            actuals: ["'#(struct:bhnode 4 () ())"],
            result: "'()",
            children: []},{name: "bhnode-right",
            formals: ["'r"],
            actuals: ["'#(struct:bhnode 4 () ())"],
            result: "'()",
            children: []},{name: "bhnode?",
            formals: ["'y"],
            actuals: ["'()"],
            result: "#f",
            children: []},{name: "bhnode?",
            formals: ["'y"],
            actuals: ["'()"],
            result: "#f",
            children: []},{name: "make-bhnode",
            formals: ["'x","'l","'r"],
            actuals: ["14","'()","'()"],
            result: "'#(struct:bhnode 14 () ())",
            children: []}]},{name: "make-heap",
            formals: ["'(list 8 4 3 9 1 6 12 14)"],
            actuals: ["'(8 4 3 9 1 6 12 14)"],
            result: "'#(struct:bhnode\n   1\n   #(struct:bhnode\n     6\n     #(struct:bhnode\n       9\n       #(struct:bhnode 14 () ())\n       ())\n     #(struct:bhnode 8 () ()))\n   #(struct:bhnode\n     3\n     #(struct:bhnode 12 () ())\n     #(struct:bhnode 4 () ())))",
            children: [{name: "make-bhnode",
            formals: ["'x","'empty","'empty"],
            actuals: ["8","'()","'()"],
            result: "'#(struct:bhnode 8 () ())",
            children: []},{name: "bhnode?",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode 8 () ())"],
            result: "#t",
            children: []},{name: "bhnode-value",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode 8 () ())"],
            result: "8",
            children: []},{name: "bhnode-left",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode 8 () ())"],
            result: "'()",
            children: []},{name: "bhnode-right",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode 8 () ())"],
            result: "'()",
            children: []},{name: "make-bhnode",
            formals: ["'x","'(insert y r)","'l"],
            actuals: ["4","'#(struct:bhnode 8 () ())","'()"],
            result: "'#(struct:bhnode\n   4\n   #(struct:bhnode 8 () ())\n   ())",
            children: []},{name: "insert",
            formals: ["'y","'r"],
            actuals: ["8","'()"],
            result: "'#(struct:bhnode 8 () ())",
            children: [{name: "make-bhnode",
            formals: ["'x","'empty","'empty"],
            actuals: ["8","'()","'()"],
            result: "'#(struct:bhnode 8 () ())",
            children: []}]},{name: "bhnode?",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   4\n   #(struct:bhnode 8 () ())\n   ())"],
            result: "#t",
            children: []},{name: "bhnode-value",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   4\n   #(struct:bhnode 8 () ())\n   ())"],
            result: "4",
            children: []},{name: "bhnode-left",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   4\n   #(struct:bhnode 8 () ())\n   ())"],
            result: "'#(struct:bhnode 8 () ())",
            children: []},{name: "bhnode-right",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   4\n   #(struct:bhnode 8 () ())\n   ())"],
            result: "'()",
            children: []},{name: "make-bhnode",
            formals: ["'x","'(insert y r)","'l"],
            actuals: ["3","'#(struct:bhnode 4 () ())","'#(struct:bhnode 8 () ())"],
            result: "'#(struct:bhnode\n   3\n   #(struct:bhnode 4 () ())\n   #(struct:bhnode 8 () ()))",
            children: []},{name: "insert",
            formals: ["'y","'r"],
            actuals: ["4","'()"],
            result: "'#(struct:bhnode 4 () ())",
            children: [{name: "make-bhnode",
            formals: ["'x","'empty","'empty"],
            actuals: ["4","'()","'()"],
            result: "'#(struct:bhnode 4 () ())",
            children: []}]},{name: "bhnode?",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   3\n   #(struct:bhnode 4 () ())\n   #(struct:bhnode 8 () ()))"],
            result: "#t",
            children: []},{name: "bhnode-value",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   3\n   #(struct:bhnode 4 () ())\n   #(struct:bhnode 8 () ()))"],
            result: "3",
            children: []},{name: "bhnode-left",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   3\n   #(struct:bhnode 4 () ())\n   #(struct:bhnode 8 () ()))"],
            result: "'#(struct:bhnode 4 () ())",
            children: []},{name: "bhnode-right",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   3\n   #(struct:bhnode 4 () ())\n   #(struct:bhnode 8 () ()))"],
            result: "'#(struct:bhnode 8 () ())",
            children: []},{name: "make-bhnode",
            formals: ["'y","'(insert x r)","'l"],
            actuals: ["3","'#(struct:bhnode\n   8\n   #(struct:bhnode 9 () ())\n   ())","'#(struct:bhnode 4 () ())"],
            result: "'#(struct:bhnode\n   3\n   #(struct:bhnode\n     8\n     #(struct:bhnode 9 () ())\n     ())\n   #(struct:bhnode 4 () ()))",
            children: []},{name: "insert",
            formals: ["'x","'r"],
            actuals: ["9","'#(struct:bhnode 8 () ())"],
            result: "'#(struct:bhnode\n   8\n   #(struct:bhnode 9 () ())\n   ())",
            children: [{name: "bhnode?",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode 8 () ())"],
            result: "#t",
            children: []},{name: "bhnode-value",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode 8 () ())"],
            result: "8",
            children: []},{name: "bhnode-left",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode 8 () ())"],
            result: "'()",
            children: []},{name: "bhnode-right",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode 8 () ())"],
            result: "'()",
            children: []},{name: "make-bhnode",
            formals: ["'y","'(insert x r)","'l"],
            actuals: ["8","'#(struct:bhnode 9 () ())","'()"],
            result: "'#(struct:bhnode\n   8\n   #(struct:bhnode 9 () ())\n   ())",
            children: []},{name: "insert",
            formals: ["'x","'r"],
            actuals: ["9","'()"],
            result: "'#(struct:bhnode 9 () ())",
            children: [{name: "make-bhnode",
            formals: ["'x","'empty","'empty"],
            actuals: ["9","'()","'()"],
            result: "'#(struct:bhnode 9 () ())",
            children: []}]}]},{name: "bhnode?",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   3\n   #(struct:bhnode\n     8\n     #(struct:bhnode 9 () ())\n     ())\n   #(struct:bhnode 4 () ()))"],
            result: "#t",
            children: []},{name: "bhnode-value",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   3\n   #(struct:bhnode\n     8\n     #(struct:bhnode 9 () ())\n     ())\n   #(struct:bhnode 4 () ()))"],
            result: "3",
            children: []},{name: "bhnode-left",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   3\n   #(struct:bhnode\n     8\n     #(struct:bhnode 9 () ())\n     ())\n   #(struct:bhnode 4 () ()))"],
            result: "'#(struct:bhnode\n   8\n   #(struct:bhnode 9 () ())\n   ())",
            children: []},{name: "bhnode-right",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   3\n   #(struct:bhnode\n     8\n     #(struct:bhnode 9 () ())\n     ())\n   #(struct:bhnode 4 () ()))"],
            result: "'#(struct:bhnode 4 () ())",
            children: []},{name: "make-bhnode",
            formals: ["'x","'(insert y r)","'l"],
            actuals: ["1","'#(struct:bhnode\n   3\n   #(struct:bhnode 4 () ())\n   ())","'#(struct:bhnode\n   8\n   #(struct:bhnode 9 () ())\n   ())"],
            result: "'#(struct:bhnode\n   1\n   #(struct:bhnode\n     3\n     #(struct:bhnode 4 () ())\n     ())\n   #(struct:bhnode\n     8\n     #(struct:bhnode 9 () ())\n     ()))",
            children: []},{name: "insert",
            formals: ["'y","'r"],
            actuals: ["3","'#(struct:bhnode 4 () ())"],
            result: "'#(struct:bhnode\n   3\n   #(struct:bhnode 4 () ())\n   ())",
            children: [{name: "bhnode?",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode 4 () ())"],
            result: "#t",
            children: []},{name: "bhnode-value",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode 4 () ())"],
            result: "4",
            children: []},{name: "bhnode-left",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode 4 () ())"],
            result: "'()",
            children: []},{name: "bhnode-right",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode 4 () ())"],
            result: "'()",
            children: []},{name: "make-bhnode",
            formals: ["'x","'(insert y r)","'l"],
            actuals: ["3","'#(struct:bhnode 4 () ())","'()"],
            result: "'#(struct:bhnode\n   3\n   #(struct:bhnode 4 () ())\n   ())",
            children: []},{name: "insert",
            formals: ["'y","'r"],
            actuals: ["4","'()"],
            result: "'#(struct:bhnode 4 () ())",
            children: [{name: "make-bhnode",
            formals: ["'x","'empty","'empty"],
            actuals: ["4","'()","'()"],
            result: "'#(struct:bhnode 4 () ())",
            children: []}]}]},{name: "bhnode?",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   1\n   #(struct:bhnode\n     3\n     #(struct:bhnode 4 () ())\n     ())\n   #(struct:bhnode\n     8\n     #(struct:bhnode 9 () ())\n     ()))"],
            result: "#t",
            children: []},{name: "bhnode-value",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   1\n   #(struct:bhnode\n     3\n     #(struct:bhnode 4 () ())\n     ())\n   #(struct:bhnode\n     8\n     #(struct:bhnode 9 () ())\n     ()))"],
            result: "1",
            children: []},{name: "bhnode-left",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   1\n   #(struct:bhnode\n     3\n     #(struct:bhnode 4 () ())\n     ())\n   #(struct:bhnode\n     8\n     #(struct:bhnode 9 () ())\n     ()))"],
            result: "'#(struct:bhnode\n   3\n   #(struct:bhnode 4 () ())\n   ())",
            children: []},{name: "bhnode-right",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   1\n   #(struct:bhnode\n     3\n     #(struct:bhnode 4 () ())\n     ())\n   #(struct:bhnode\n     8\n     #(struct:bhnode 9 () ())\n     ()))"],
            result: "'#(struct:bhnode\n   8\n   #(struct:bhnode 9 () ())\n   ())",
            children: []},{name: "make-bhnode",
            formals: ["'y","'(insert x r)","'l"],
            actuals: ["1","'#(struct:bhnode\n   6\n   #(struct:bhnode 8 () ())\n   #(struct:bhnode 9 () ()))","'#(struct:bhnode\n   3\n   #(struct:bhnode 4 () ())\n   ())"],
            result: "'#(struct:bhnode\n   1\n   #(struct:bhnode\n     6\n     #(struct:bhnode 8 () ())\n     #(struct:bhnode 9 () ()))\n   #(struct:bhnode\n     3\n     #(struct:bhnode 4 () ())\n     ()))",
            children: []},{name: "insert",
            formals: ["'x","'r"],
            actuals: ["6","'#(struct:bhnode\n   8\n   #(struct:bhnode 9 () ())\n   ())"],
            result: "'#(struct:bhnode\n   6\n   #(struct:bhnode 8 () ())\n   #(struct:bhnode 9 () ()))",
            children: [{name: "bhnode?",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   8\n   #(struct:bhnode 9 () ())\n   ())"],
            result: "#t",
            children: []},{name: "bhnode-value",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   8\n   #(struct:bhnode 9 () ())\n   ())"],
            result: "8",
            children: []},{name: "bhnode-left",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   8\n   #(struct:bhnode 9 () ())\n   ())"],
            result: "'#(struct:bhnode 9 () ())",
            children: []},{name: "bhnode-right",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   8\n   #(struct:bhnode 9 () ())\n   ())"],
            result: "'()",
            children: []},{name: "make-bhnode",
            formals: ["'x","'(insert y r)","'l"],
            actuals: ["6","'#(struct:bhnode 8 () ())","'#(struct:bhnode 9 () ())"],
            result: "'#(struct:bhnode\n   6\n   #(struct:bhnode 8 () ())\n   #(struct:bhnode 9 () ()))",
            children: []},{name: "insert",
            formals: ["'y","'r"],
            actuals: ["8","'()"],
            result: "'#(struct:bhnode 8 () ())",
            children: [{name: "make-bhnode",
            formals: ["'x","'empty","'empty"],
            actuals: ["8","'()","'()"],
            result: "'#(struct:bhnode 8 () ())",
            children: []}]}]},{name: "bhnode?",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   1\n   #(struct:bhnode\n     6\n     #(struct:bhnode 8 () ())\n     #(struct:bhnode 9 () ()))\n   #(struct:bhnode\n     3\n     #(struct:bhnode 4 () ())\n     ()))"],
            result: "#t",
            children: []},{name: "bhnode-value",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   1\n   #(struct:bhnode\n     6\n     #(struct:bhnode 8 () ())\n     #(struct:bhnode 9 () ()))\n   #(struct:bhnode\n     3\n     #(struct:bhnode 4 () ())\n     ()))"],
            result: "1",
            children: []},{name: "bhnode-left",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   1\n   #(struct:bhnode\n     6\n     #(struct:bhnode 8 () ())\n     #(struct:bhnode 9 () ()))\n   #(struct:bhnode\n     3\n     #(struct:bhnode 4 () ())\n     ()))"],
            result: "'#(struct:bhnode\n   6\n   #(struct:bhnode 8 () ())\n   #(struct:bhnode 9 () ()))",
            children: []},{name: "bhnode-right",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   1\n   #(struct:bhnode\n     6\n     #(struct:bhnode 8 () ())\n     #(struct:bhnode 9 () ()))\n   #(struct:bhnode\n     3\n     #(struct:bhnode 4 () ())\n     ()))"],
            result: "'#(struct:bhnode\n   3\n   #(struct:bhnode 4 () ())\n   ())",
            children: []},{name: "make-bhnode",
            formals: ["'y","'(insert x r)","'l"],
            actuals: ["1","'#(struct:bhnode\n   3\n   #(struct:bhnode 12 () ())\n   #(struct:bhnode 4 () ()))","'#(struct:bhnode\n   6\n   #(struct:bhnode 8 () ())\n   #(struct:bhnode 9 () ()))"],
            result: "'#(struct:bhnode\n   1\n   #(struct:bhnode\n     3\n     #(struct:bhnode 12 () ())\n     #(struct:bhnode 4 () ()))\n   #(struct:bhnode\n     6\n     #(struct:bhnode 8 () ())\n     #(struct:bhnode 9 () ())))",
            children: []},{name: "insert",
            formals: ["'x","'r"],
            actuals: ["12","'#(struct:bhnode\n   3\n   #(struct:bhnode 4 () ())\n   ())"],
            result: "'#(struct:bhnode\n   3\n   #(struct:bhnode 12 () ())\n   #(struct:bhnode 4 () ()))",
            children: [{name: "bhnode?",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   3\n   #(struct:bhnode 4 () ())\n   ())"],
            result: "#t",
            children: []},{name: "bhnode-value",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   3\n   #(struct:bhnode 4 () ())\n   ())"],
            result: "3",
            children: []},{name: "bhnode-left",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   3\n   #(struct:bhnode 4 () ())\n   ())"],
            result: "'#(struct:bhnode 4 () ())",
            children: []},{name: "bhnode-right",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   3\n   #(struct:bhnode 4 () ())\n   ())"],
            result: "'()",
            children: []},{name: "make-bhnode",
            formals: ["'y","'(insert x r)","'l"],
            actuals: ["3","'#(struct:bhnode 12 () ())","'#(struct:bhnode 4 () ())"],
            result: "'#(struct:bhnode\n   3\n   #(struct:bhnode 12 () ())\n   #(struct:bhnode 4 () ()))",
            children: []},{name: "insert",
            formals: ["'x","'r"],
            actuals: ["12","'()"],
            result: "'#(struct:bhnode 12 () ())",
            children: [{name: "make-bhnode",
            formals: ["'x","'empty","'empty"],
            actuals: ["12","'()","'()"],
            result: "'#(struct:bhnode 12 () ())",
            children: []}]}]},{name: "bhnode?",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   1\n   #(struct:bhnode\n     3\n     #(struct:bhnode 12 () ())\n     #(struct:bhnode 4 () ()))\n   #(struct:bhnode\n     6\n     #(struct:bhnode 8 () ())\n     #(struct:bhnode 9 () ())))"],
            result: "#t",
            children: []},{name: "bhnode-value",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   1\n   #(struct:bhnode\n     3\n     #(struct:bhnode 12 () ())\n     #(struct:bhnode 4 () ()))\n   #(struct:bhnode\n     6\n     #(struct:bhnode 8 () ())\n     #(struct:bhnode 9 () ())))"],
            result: "1",
            children: []},{name: "bhnode-left",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   1\n   #(struct:bhnode\n     3\n     #(struct:bhnode 12 () ())\n     #(struct:bhnode 4 () ()))\n   #(struct:bhnode\n     6\n     #(struct:bhnode 8 () ())\n     #(struct:bhnode 9 () ())))"],
            result: "'#(struct:bhnode\n   3\n   #(struct:bhnode 12 () ())\n   #(struct:bhnode 4 () ()))",
            children: []},{name: "bhnode-right",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   1\n   #(struct:bhnode\n     3\n     #(struct:bhnode 12 () ())\n     #(struct:bhnode 4 () ()))\n   #(struct:bhnode\n     6\n     #(struct:bhnode 8 () ())\n     #(struct:bhnode 9 () ())))"],
            result: "'#(struct:bhnode\n   6\n   #(struct:bhnode 8 () ())\n   #(struct:bhnode 9 () ()))",
            children: []},{name: "make-bhnode",
            formals: ["'y","'(insert x r)","'l"],
            actuals: ["1","'#(struct:bhnode\n   6\n   #(struct:bhnode\n     9\n     #(struct:bhnode 14 () ())\n     ())\n   #(struct:bhnode 8 () ()))","'#(struct:bhnode\n   3\n   #(struct:bhnode 12 () ())\n   #(struct:bhnode 4 () ()))"],
            result: "'#(struct:bhnode\n   1\n   #(struct:bhnode\n     6\n     #(struct:bhnode\n       9\n       #(struct:bhnode 14 () ())\n       ())\n     #(struct:bhnode 8 () ()))\n   #(struct:bhnode\n     3\n     #(struct:bhnode 12 () ())\n     #(struct:bhnode 4 () ())))",
            children: []},{name: "insert",
            formals: ["'x","'r"],
            actuals: ["14","'#(struct:bhnode\n   6\n   #(struct:bhnode 8 () ())\n   #(struct:bhnode 9 () ()))"],
            result: "'#(struct:bhnode\n   6\n   #(struct:bhnode\n     9\n     #(struct:bhnode 14 () ())\n     ())\n   #(struct:bhnode 8 () ()))",
            children: [{name: "bhnode?",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   6\n   #(struct:bhnode 8 () ())\n   #(struct:bhnode 9 () ()))"],
            result: "#t",
            children: []},{name: "bhnode-value",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   6\n   #(struct:bhnode 8 () ())\n   #(struct:bhnode 9 () ()))"],
            result: "6",
            children: []},{name: "bhnode-left",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   6\n   #(struct:bhnode 8 () ())\n   #(struct:bhnode 9 () ()))"],
            result: "'#(struct:bhnode 8 () ())",
            children: []},{name: "bhnode-right",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode\n   6\n   #(struct:bhnode 8 () ())\n   #(struct:bhnode 9 () ()))"],
            result: "'#(struct:bhnode 9 () ())",
            children: []},{name: "make-bhnode",
            formals: ["'y","'(insert x r)","'l"],
            actuals: ["6","'#(struct:bhnode\n   9\n   #(struct:bhnode 14 () ())\n   ())","'#(struct:bhnode 8 () ())"],
            result: "'#(struct:bhnode\n   6\n   #(struct:bhnode\n     9\n     #(struct:bhnode 14 () ())\n     ())\n   #(struct:bhnode 8 () ()))",
            children: []},{name: "insert",
            formals: ["'x","'r"],
            actuals: ["14","'#(struct:bhnode 9 () ())"],
            result: "'#(struct:bhnode\n   9\n   #(struct:bhnode 14 () ())\n   ())",
            children: [{name: "bhnode?",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode 9 () ())"],
            result: "#t",
            children: []},{name: "bhnode-value",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode 9 () ())"],
            result: "9",
            children: []},{name: "bhnode-left",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode 9 () ())"],
            result: "'()",
            children: []},{name: "bhnode-right",
            formals: ["'h"],
            actuals: ["'#(struct:bhnode 9 () ())"],
            result: "'()",
            children: []},{name: "make-bhnode",
            formals: ["'y","'(insert x r)","'l"],
            actuals: ["9","'#(struct:bhnode 14 () ())","'()"],
            result: "'#(struct:bhnode\n   9\n   #(struct:bhnode 14 () ())\n   ())",
            children: []},{name: "insert",
            formals: ["'x","'r"],
            actuals: ["14","'()"],
            result: "'#(struct:bhnode 14 () ())",
            children: [{name: "make-bhnode",
            formals: ["'x","'empty","'empty"],
            actuals: ["14","'()","'()"],
            result: "'#(struct:bhnode 14 () ())",
            children: []}]}]}]}]}]}