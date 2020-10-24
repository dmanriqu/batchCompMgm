#---------------------------------------------------------------------------
# Create R package Makefile
#---------------------------------------------------------------------------
R_PACK      = batchCompMgm

R_CODE1_LOC = R
R_CODE1 = paramComp.R batchComp.R taskLog.R general.R

R_MAN1_LOC = man
R_MAN1 = 

R_META_LOC = meta
R_META = DESCRIPTION NAMESPACE

#--------------------------------------------------------------------------------
PACK_ROOT   = output/$(R_PACK)
R_META_DEST = $(PACK_ROOT)
R_CODE_DEST = $(PACK_ROOT)/R
R_MAN_DEST  = $(PACK_ROOT)/man
R_DATA_DEST = $(PACK_ROOT)/data
C_CODE_DEST = $(PACK_ROOT)/src

VERSION = $(shell cat data/version.txt)
VERSION_ESC = $(shell $VERSION | sed --posix -E 's/\./\\\./g') 
DATE = $(shell date +'%F')

ALL_FILES = $(addprefix $(R_CODE_DEST)/, $(R_CODE1) $(R_CODE2))  \
	$(addprefix $(R_MAN_DEST)/, $(R_MAN1) $(R_MAN2)) \
	$(addprefix $(R_DATA_DEST)/, $(R_DATA)) \
	$(addprefix $(C_CODE_DEST)/, $(C_CODE)) \
	$(addprefix $(R_META_DEST)/, $(R_META))

TEXT_FILE_EXT = *.R *.cpp *.Rd *.h DESCRIPTION NAMESPACE

.PHONY: copy check install clean all

all: $(ALL_FILES) copy output/version.txt output/$(R_PACK)

copy : $(ALL_FILES)
	cd $(PACK_ROOT); sed --posix -E -i -e 's/<date>/$(DATE)/g' -e 's/<version>/$(VERSION)/g' -e 's/<package_name>/$(R_PACK)/g' DESCRIPTION

output/version.txt output/$(R_PACK) : $(ALL_FILES)
	Rscript --vanilla -e 'roxygen2::roxygenize(package.dir = "$(PACK_ROOT)")'
	cd output; R CMD build $(R_PACK)
	cp data/version.txt output

dos2unix : $(ALL_FILES)
	find $(PACK_ROOT) -name '*.R' -o -name '*.cpp' -o -name '*.h' -o -name '*.Rd' -exec dos2unix {} \;

$(addprefix $(R_CODE_DEST)/, $(R_CODE1)): $(R_CODE_DEST)/% : $(R_CODE1_LOC)/% | $(R_CODE_DEST)/ cache/
	cp -f $< $@; data/AddHeader.sh $@
	

$(addprefix $(R_CODE_DEST)/, $(R_CODE2)): $(R_CODE_DEST)/% : $(R_CODE2_LOC)/% | $(R_CODE_DEST)/ cache/
	cp -f $< $@; data/AddHeader.sh $@

$(addprefix $(R_MAN_DEST)/, $(R_MAN1)) : $(R_MAN_DEST)/% : $(R_MAN1_LOC)/% | $(R_MAN_DEST)/ cache/
	cp -f $< $@; data/AddHeader.sh $@

$(addprefix $(R_MAN_DEST)/, $(R_MAN2)) : $(R_MAN_DEST)/% : $(R_MAN2_LOC)/% | $(R_MAN_DEST)/ cache/
	cp -f $< $@; data/AddHeader.sh $@

ifeq ($(OS),Windows_NT)
$(addprefix $(R_DATA_DEST)/, $(R_DATA)) : $(R_DATA_DEST)/%.RData : $(R_DATA_LOC)/%.R | $(R_DATA_DEST)/ cache/
	Rscript --vanilla -e $$'$* <- dget(file = \'$<\')' -e $$'save($*, file=\'$@\')'
else
$(addprefix $(R_DATA_DEST)/, $(R_DATA)) : $(R_DATA_DEST)/%.RData : $(R_DATA_LOC)/%.R | $(R_DATA_DEST)/ cache/
	Rscript --vanilla -e '$* <- dget(file = "$<")' -e 'save($*, file="$@")'
endif

$(addprefix $(C_CODE_DEST)/, $(C_CODE)) : $(C_CODE_DEST)/% : $(C_CODE_LOC)/% | $(C_CODE_DEST)/ $(C_CODE_DEST)/daniel2/ $(C_CODE_DEST)/R_Interface/ cache/
	cp -f $< $@; data/AddHeader.sh $@

$(addprefix $(R_META_DEST)/, $(R_META)) : $(R_META_DEST)/% : $(R_META_LOC)/% | $(R_META_DEST)/ cache/
	cp -f $< $@; data/AddHeader.sh $@

%/ :
	mkdir -p $@

check: | cache/
	cp output/$(R_PACK)_$(VERSION).tar.gz cache/
	cd cache; R CMD check $(R_PACK)_$(VERSION).tar.gz

install: | cache/
	cp output/$(R_PACK)_$(VERSION).tar.gz cache/
	R CMD INSTALL -l cache cache/$(R_PACK)_$(VERSION).tar.gz

clean :
	rm -rf output/
	rm -rf cache/
	rm -f *.lock

