(load "garnet/alpha/gadgets/error-gadget-loader")
(load "garnet/alpha/gadgets/labeled-box-loader")
(load "garnet/alpha/gadgets/arrow-line-loader")
(load garnet-c32-loader)
(compile-file "~bvz/garnet/constraint-gadget/defs")
(compile-file "~bvz/garnet/constraint-gadget/support-constraints")
(compile-file "~bvz/garnet/constraint-gadget/attach-constraints")
(compile-file "~bvz/garnet/constraint-gadget/box-parts")
(compile-file "~bvz/garnet/constraint-gadget/box")
(compile-file "~bvz/garnet/constraint-gadget/support-box-constraints")
(compile-file "~bvz/garnet/constraint-gadget/line-constraint-defs")
(compile-file "~bvz/garnet/constraint-gadget/line-constraint-defs")
(compile-file "~bvz/garnet/constraint-gadget/line-constraint-objs")
(compile-file "~bvz/garnet/constraint-gadget/new-line-constraint")
(compile-file "~bvz/garnet/constraint-gadget/set-feedback")
(compile-file "~bvz/garnet/constraint-gadget/custom")
(load "~bvz/garnet/constraint-gadget/tester.lisp")
