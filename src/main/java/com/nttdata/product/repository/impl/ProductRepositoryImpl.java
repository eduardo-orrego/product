package com.nttdata.product.repository.impl;


import com.nttdata.product.model.Product;
import com.nttdata.product.repository.ProductReactiveMongodb;
import com.nttdata.product.repository.ProductRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@Slf4j
@Repository
public class ProductRepositoryImpl implements ProductRepository {

    @Autowired
    private ProductReactiveMongodb productReactiveMongodb;

    @Override
    public Flux<Product> findProducts(String type) {
        return productReactiveMongodb.findByType(type)
            .doOnComplete(() -> log.info("Successful find - type: ".concat(type)));
    }

    @Override
    public Mono<Product> findProduct(String productId){
        return productReactiveMongodb.findById(productId)
            .doOnSuccess(product -> log.info("Successful find - productId: ".concat(productId)));
    }

    @Override
    public Mono<Boolean> findExistsProduct(String productId){
        return productReactiveMongodb.existsById(productId)
            .doOnSuccess(exists -> log.info("Successful find exists - productId: ".concat(productId)));
    }

    @Override
    public Mono<Boolean> findExistsProducts(String type) {
        return productReactiveMongodb.existsByType(type)
            .doOnSuccess(exists -> log.info("Successful find exists - type: ".concat(type)));
    }

    @Override
    public Mono<Product> saveProduct(Product product){
        return productReactiveMongodb.save(product)
            .doOnSuccess(result -> log.info("Successful save - type: ".concat(result.getId())));
    }

    @Override
    public Mono<Void> deleteProduct(String productId){
        return productReactiveMongodb.deleteById(productId)
            .doOnSuccess(result -> log.info("Successful delete - type: ".concat(productId)));
    }

}
