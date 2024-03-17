package com.nttdata.product.repository;

import com.nttdata.product.model.Product;
import org.springframework.data.mongodb.repository.ReactiveMongoRepository;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Mono;

@Repository
public interface ProductReactiveMongodb extends ReactiveMongoRepository<Product, String> {

    Mono<Boolean> existsByType(String type);

    Mono<Product> findByType(String type);

}

