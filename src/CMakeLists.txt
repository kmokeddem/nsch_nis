project (src)

include_directories (${CMAKE_CURRENT_SOURCE_DIR} .. )

add_executable(nsch_nis
               nsch_nis.cpp
               )

target_link_libraries(nsch_nis)
