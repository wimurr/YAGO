import re
from pathlib import Path
import time

DEFAULT_DIR = "/Users/umurrwi/Documents/Projects/Ontologies/YAGO/YAGO2/yago2s_ttl-1.7z/"

'''
Cleans up typical RDF lines, such as the following:

<Abraham_Lincoln>	rdf:type	<wikicategory_Union_political_leaders> .

so swi-prolog rdf/sembweb's rdf_load can read them.

Skips the files specified, e.g., we never need to know the source of the facts from yagoSources, which is
a HUGE file.

max_lines or max_files are used just during debugging, set to None otherwise.
'''

def cleanup(dir=DEFAULT_DIR, pattern='yago*.ttl', files_to_skip=['yagoSources.ttl'],trace_file_name='label_cleaning_report.txt',
            max_lines=None, max_files=None, tracing=False):
    start_time = time.time()
    dir_path = Path(dir)
    files_read = 0 # files

    cleaning_trace_file = Path(dir_path / trace_file_name)
    with open(cleaning_trace_file,'w') as log_cleaning_messages:
        for filepath in dir_path.glob(pattern):
            short_name = filepath.name
            if short_name in files_to_skip:
                print(f"Skipping over file {short_name}, as it is one of the files to exclude.")
                continue
            cleaned_file_name = f"cleaned_{short_name}"
            files_read += 1
            if max_files and (files_read > max_files):
                print(f"Finished {max_files} files for testing.")
                break            
            with open(filepath,'r') as file_to_clean:
                print(f"\n\nCleaning file: {short_name} to {cleaned_file_name}\n\n")
                cleaned_file_path = Path(dir_path / cleaned_file_name)
                i = 0 # line number in current file
                with open(cleaned_file_path,'w') as cleaned_file:
                    for line in file_to_clean:
                        i += 1
                        if max_lines and (i > max_lines):
                            print(f"Finished {max_lines} lines for testing.")
                            break
                        line = line.strip(" \n")
                        if line.startswith('#@'):
                            # Skip these lines with the fact ID number
                            continue
                        if len(line)==0 or line[0] in ['#','@']:
                            cleaned_file.write(line + '\n')
                            continue
                        triple_parts = line.strip(" .\n").split('\t')
                        assert len(triple_parts)==3,f"Line read in: '{line}', does not have three parts after trimming."
                        subject,predicate,object = triple_parts
                        clean_subject = clean_resource(subject)
                        clean_predicate = clean_resource(predicate)
                        clean_object = clean_resource(object)
                        if clean_subject is None or clean_predicate is None or clean_object is None:
                            # Skip lines with resources where the tag is so bad we cannot clean it
                            log_cleaning_messages.write(f"Dropped line {i}: '{line} in file {short_name}. One or more tags cannot be cleaned.\n'")
                            continue
                        cleaned_line = clean_subject + '\t' + clean_predicate + '\t' + clean_object + ' .'
                        if tracing and line != cleaned_line:
                            print(f"Changed line {i}: Was '{line}'")
                            print('-->')
                            print(f"Now: '{cleaned_line}'")                        
                            print('\n')
                        cleaned_file.write(cleaned_line + '\n')
    end_time = time.time()
    print(f"Took {end_time - start_time:.2f} sec.")

# re.sub(r'''["`']''','',"<Csaba_B'a`log>")

# chars that indicate a hopeless tag and fact,
# such as a left bracket inside a tag.
chars_to_abort_on = re.compile(r'[{}<>]')

# chars that are simply stripped out
# to include apostrophe: chars_to_strip_out = re.compile(r'''["`']''')
chars_to_strip_out = re.compile(r'["`{}<>]')

# char and word replacements that are performed,
# --These can replace characters with HTML codes,
# --Or replace one word with another.
# To add forward slash: ('/','&#47;')
# To add backward slash: ('\','&#92;')
replacements = [('m^2','m**2'),('\n',''),('</','_'),('^','&#94;'),('\\','&#92;')]

unicode_escape_sequence = re.compile(r'\\[0-9A-Fa-f]{4}')

multiple_underscore_sequence = re.compile(r'_+')

example_resources_to_clean = [('<In_Your_Arms_(Love_song_from_"Neighbours")>','<In_Your_Arms_(Love_song_from_Neighbours)>'),
                                                   ('"7630000.0"^^<m^2>','"7630000.0"^^<m**2>'),
                                                   ('<wikicategory_Amusement_parks_in_{{{location2}}}>','<wikicategory_Amusement_parks_in_location2>'),
                                                   ('<Sufyan_ibn_`Uyaynah>','<Sufyan_ibn_Uyaynah>'),
                                                   ('<wikicategory_People_from_Ballarat</text\u003e\n______<sha1_/\u003e\n____</revision\u003e\n__</page>','<wikicategory_People_from_Ballarat/text_sha1_/_/revision_/page>'),
                                                   ('<startedOnDate >','<startedOnDate>',)]

def test_resource_cleanup():
    for resource,target_cleaned_resource in example_resources_to_clean:
        cleaned_up = clean_resource(resource,tracing=True)

        if resource[0]!='<' or resource[-1]!='>':
            print(f"WARNING: test_resource_cleanup only tests to see if resources are cleaned properly, this either does not start with or end with angle brackets: '{resource}'.")

        if target_cleaned_resource[0]!='<' or target_cleaned_resource[-1]!='>':
            print(f"WARNING: test_resource_cleanup only tests to see if resources are cleaned properly, this either does not start with or end with angle brackets: '{target_cleaned_resource}'.")            

        if cleaned_up == target_cleaned_resource:
            print(f"SUCCESS: Cleaned '{resource}' to '{target_cleaned_resource}'")
        else:
            print(f"FAILED: Cleaned '{resource}' to:\n'{cleaned_up}', instead of to:\n '{target_cleaned_resource}'")
        print('DONE!')

def clean_resource(resource, tracing=False):
    cleaned_resource = resource

    if resource[0]=='<' and resource[-1]=='>':

        # It is easier to only search on the substring within the tag
        # for chars that are illegal and to make replacements.
        inside_tag = resource[1:-1]

        # Strip out chars that should not be inside the tag
        cleaned_resource = chars_to_strip_out.sub('',inside_tag)
        if tracing and cleaned_resource != inside_tag:
            print(f"After stripping chars, changed  '<{inside_tag}>' to '<{cleaned_resource}>'.")
        partially_cleaned = cleaned_resource

        # Make replacements for other chars or words as needed
        # N.B. We allow regular expressions in pat.
        for pat,to_replace in replacements:
            if pat in partially_cleaned:
                after_current_replacements = partially_cleaned.replace(pat,to_replace)
                if tracing and partially_cleaned != after_current_replacements:
                    print(f"After replacing '{pat}' for '{to_replace}', changed  '{resource}' to '{cleaned_resource}'.")
                partially_cleaned = after_current_replacements

        # Shrink multiple underscores to only one.
        partially_cleaned = multiple_underscore_sequence.sub('_',partially_cleaned)

        # We can't use this fact at all if we detect certain chars
        # in the inner part of the tag. Note we only search INSIDE
        # the tag
        if chars_to_abort_on.search(partially_cleaned):
            return None

        # Strip off any extraneous spaces at the front or back of the tag
        partially_cleaned = partially_cleaned.strip()

        # After stripping out bad chars, making replacements,
        # and tossing unworkable tags, we sandwich the chars
        # inside the tag back between angle brackets.
        cleaned_resource = '<' + partially_cleaned + '>'
    else:
        cleaned_resource = clean_literal(resource)

    if tracing and cleaned_resource != resource:
        print(f"Changed  '{resource}' to '{cleaned_resource}'.")

    return cleaned_resource
    
'''
clean_literal handles the literals in triples such as those shown here:

<Nongo-Souala>	<hasLatitude>	"11.755"^^<degrees> .
<Regional_Municipality_of_Waterloo>	<hasPopulationDensity>	"370.4"^^</km**2> .
<Matt_Murphy_(English_footballer)>	<hasHeight>	"1.778"^^<m> .
<Normanton_le_Heath>	<hasNumberOfPeople>	"131"^^xsd:nonNegativeInteger .
<Tang_Guoqiang>	<wasBornOnDate>	"1952-05-04"^^xsd:date .
<St._Ann,_Missouri>	<hasLatitude>	"38.727222222222224"^^<degrees> .

Currently, the only change it makes is to replace the string "m^2" with "m**2".
'''

def clean_literal(resource, tracing=False):
    if "m^2" in resource:
        return resource.replace("m^2","m**2")
    else:
        return resource
    

            



