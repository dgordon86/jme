OBJS = ast.cmo parser.cmo scanner.cmo interpret.cmo datatypes.cmo util.cmo bytecode.cmo compile.cmo execute.cmo jme.cmo

TESTS = \
arith1 \
arith2 \
fib \
for1 \
func1 \
func2 \
func3 \
gcd \
global1 \
hello \
if1 \
if2 \
if3 \
if4 \
ops1 \
var1 \
while1

TARFILES = Makefile testall.sh scanner.mll parser.mly \
	ast.ml bytecode.ml interpret.ml compile.ml execute.ml datatypes.ml util.ml jme.ml \
	$(TESTS:%=tests/test-%.jme) \
	$(TESTS:%=tests/test-%.out)

jme : $(OBJS)
	ocamlc -o jme $(OBJS)

.PHONY : test
test : jme testall.sh
	./testall.sh

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	ocamlyacc parser.mly

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

jme.tar.gz : $(TARFILES)
	cd .. && tar czf jme/jme.tar.gz $(TARFILES:%=jme/%)

.PHONY : clean
clean :
	rm -f jme parser.ml parser.mli scanner.ml testall.log \
	*.cmo *.cmi *.out *.diff

# Generated by ocamldep *.ml *.mli
ast.cmo: 
ast.cmx: 
bytecode.cmo: datatypes.cmo ast.cmo 
bytecode.cmx: datatypes.cmx ast.cmx 
compile.cmo: util.cmo datatypes.cmo bytecode.cmo ast.cmo 
compile.cmx: util.cmx datatypes.cmx bytecode.cmx ast.cmx 
datatypes.cmo :
datatypes.cmx :
execute.cmo: util.cmo datatypes.cmo bytecode.cmo ast.cmo 
execute.cmx: util.cmx datatypes.cmx bytecode.cmx ast.cmx 
interpret.cmo: ast.cmo 
interpret.cmx: ast.cmx 
jme.cmo: scanner.cmo parser.cmi interpret.cmo execute.cmo compile.cmo \
    bytecode.cmo ast.cmo util.cmi
jme.cmx: scanner.cmx parser.cmx interpret.cmx execute.cmx compile.cmx \
    bytecode.cmx ast.cmx util.cmx
parser.cmo: ast.cmo parser.cmi 
parser.cmx: ast.cmx parser.cmi 
scanner.cmo: parser.cmi 
scanner.cmx: parser.cmx 
parser.cmi: ast.cmo 
util.cmo: datatypes.cmo ast.cmo
util.cmx: datatypes.cmx ast.cmx 