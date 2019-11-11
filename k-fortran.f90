module lib
    implicit None

contains
    subroutine get_arguments(iterations, filename_input, n_samples, n_clusters, n_features)
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

    subroutine read_samples(filename_input, samples, assignments, n_samples, n_features)
        character(len = :), allocatable, intent(in) :: filename_input
        integer, intent(in) :: n_samples, n_features
        real, dimension(:, :), allocatable, intent(out) :: samples
        integer, dimension(:), allocatable, intent(out) :: assignments
        integer :: current_line

        allocate(samples(n_samples, n_features))
        allocate(assignments(n_samples))

        open(unit = 11, file = filename_input, status = "old")
        do current_line = 1, n_samples
            read(11, *) samples(current_line, :)
        end do
        close(11)
    end subroutine read_samples

    subroutine initialize_centroids(samples, centroids, n_samples, n_clusters, n_features)
        integer, intent(in) :: n_samples, n_clusters, n_features
        real, dimension(:, :), allocatable, intent(in) :: samples
        real, dimension(:, :), allocatable, intent(out) :: centroids
        real, dimension(n_features) :: mini, maxi
        integer :: current_centroid, current_feature
        real :: random_num

        allocate(centroids(n_clusters, n_features))

        do current_feature = 1, n_features
            mini(current_feature) = minval(samples(:, current_feature))
            maxi(current_feature) = maxval(samples(:, current_feature))
        end do

        do current_centroid = 1, n_clusters
            do current_feature = 1, n_features
                call random_number(random_num)
                centroids(current_centroid, current_feature) = &
                    mini(current_feature) + (maxi(current_feature) + 1 - mini(current_feature)) * random_num
            end do
        end do
    end subroutine initialize_centroids
end module lib

program main
    use lib

    implicit None

    real :: start, finish
    integer :: iterations, n_samples, n_features, n_clusters
    character(len = :), allocatable :: filename_input
    real, dimension(:, :), allocatable :: samples, centroids
    integer, dimension(:), allocatable :: assignments

    ! Parse the arguments into the variables
    call get_arguments(iterations, filename_input, n_samples, n_clusters, n_features)
    ! Read the input file into the samples array and initialize the assignments array
    call read_samples(filename_input, samples, assignments, n_samples, n_features)

    print *, "Performing k-means clustering on ", filename_input, " with ",&
        iterations, " iteration(s)"

    call initialize_centroids(samples, centroids, n_samples, n_clusters, n_features)

end program main
