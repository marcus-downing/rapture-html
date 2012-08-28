SCALAHOME:=/opt/scala-2.10.0-latest/bin

all: package

compile:
	@mkdir -p bin
	@echo Compiling...
	@$(SCALAHOME)/scalac -cp lib/rapture-io.jar -language:postfixOps,implicitConversions -feature -usejavacp -unchecked -deprecation -d bin src/*.scala

doc:
	@mkdir -p doc
	@echo Generating API documentation...
	@$(SCALAHOME)/scaladoc -usejavacp -unchecked -deprecation -d doc src/*.scala

package: compile
	@echo Packaging rapture-html.jar
	@jar cmf etc/manifest rapture-html.jar -C bin rapture

clean:
	@rm -fr bin
	@rm -f rapture-html.jar
	@echo Cleaned bin and rapture-html.jar

int:
	@$(SCALAHOME)/scala -cp rapture-html.jar

.PHONY: compile package clean
