




tools_program_names <- function() {
  c("IARC/IACR Check", "IARC/IACR Multiple Primary",
    "ICD-O-3 -> ICD-10")
}




tools_program_guides <- function(program.name) {
  assert_tools_program(program.name)
  
  switch(
    program.name,
    
    `IARC/IACR Check` = {
      data.frame(
        keystrokes = c("ALT + T", "C"),
        explained = c("Select 'Tools'", "Select 'IARC/IACR Check'")
      )
    },
    
    `ICD-O-3 -> ICD-10` = {
      data.frame(
        keystrokes = c("ALT + C", "3"),
        explained = c("Select 'Conversions'", "Select 'ICD-O-3 -> ICD-10'")
      )
    }
  )
  
}





tools_program_keystrokes <- function(program.name) {
  assert_tools_program(program.name)
  
  guide_df <- tools_program_guides(program.name)
  guide_df[["keystrokes"]]
  
}










