module lib
    implicit None

contains
    subroutine get_arguments (iterations, filename_input, n_samples, n_clusters, n_features)
        character(len = :), allocatable, intent(out) :: filename_input
        integer, intent(out) :: iterations, n_samples, n_features, n_clusters
        character(len = :), allocatable :: iterations_char, clusters_char, samples_char, features_char
        integer :: arg_length

        if (command_argument_count().ne.5) then
            print '("Usage: k-fortran.f90 <iterations> <input file> <number of samples> <number of clusters> <number of features>")'
            stop
        end if

        call get_command_argument(number = 1, length = arg_length)
        allocate (character(len = arg_length) :: iterations_char)
        call get_command_argument(number = 1, value = iterations_char)

        call get_command_argument(number = 2, length = arg_length)
        allocate (character(len = arg_length) :: filename_input)
        call get_command_argument(number = 2, value = filename_input)

        call get_command_argument(number = 3, length = arg_length)
        allocate (character(len = arg_length) :: samples_char)
        call get_command_argument(number = 3, value = samples_char)

        call get_command_argument(number = 4, length = arg_length)
        allocate (character(len = arg_length) :: clusters_char)
        call get_command_argument(number = 4, value = clusters_char)

        call get_command_argument(number = 5, length = arg_length)
        allocate (character(len = arg_length) :: features_char)
        call get_command_argument(number = 5, value = features_char)

        read(iterations_char, *) iterations
        read(samples_char, *) n_samples
        read(clusters_char, *) n_clusters
        read(features_char, *) n_features
    end subroutine get_arguments

end module lib

program main
    use lib

    implicit None

    real :: start, finish
    integer :: iterations, n_samples, n_features, n_clusters
    character(len = :), allocatable :: filename_input
    real, dimension(:, :), allocatable :: points, centroids
    integer, dimension(:), allocatable :: assignments

    call get_arguments(iterations, filename_input, n_samples, n_clusters, n_features)

end program main
