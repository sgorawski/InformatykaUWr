package dbapp.project.common

interface Repository<T> {
    fun create(new: T): Boolean
    fun find(id: Int): T?
    fun findAll(): Collection<T>
    fun delete(id: Int): Boolean
}